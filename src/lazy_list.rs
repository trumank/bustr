use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::{Modifier, Style},
    widgets::{List, ListItem, ListState, StatefulWidget},
};

pub trait ListItemProducer<'a> {
    fn total(&self) -> usize;
    fn items(&self, range: std::ops::Range<usize>) -> impl ExactSizeIterator<Item = ListItem<'a>>;
}

pub struct LazyList<P> {
    item_producer: P,
}

impl<'a, P> LazyList<P>
where
    P: ListItemProducer<'a>,
{
    pub fn new(item_producer: P) -> Self {
        Self { item_producer }
    }
}

impl<'a, P> StatefulWidget for LazyList<P>
where
    P: ListItemProducer<'a>,
{
    type State = ListState;

    fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        let visible_height = area.height as usize;

        let total = self.item_producer.total();

        // Fix up offset so it's within the non-virtual range of the selected item
        if let Some(selected) = state.selected() {
            *state.offset_mut() = state.offset().clamp(
                selected.saturating_sub(visible_height),
                selected.saturating_add(visible_height),
            )
        }
        // Also clamp offset to bottom to prevent scrolling past visible lines
        *state.offset_mut() = state.offset().min(total.saturating_sub(visible_height));

        let scroll = state.offset();

        // Pag page height above and below visible
        let pad = visible_height;

        // Calculate visible range
        let start_idx = scroll.saturating_sub(pad);

        // Lines before visible start
        let pre_pad = scroll - start_idx;
        let offset = scroll - pre_pad;

        let end_idx = (start_idx + visible_height + pad + pre_pad).min(total);

        // Create visible items
        let items: Vec<ListItem> = self.item_producer.items(start_idx..end_idx).collect();

        // Create and render the list
        let list = List::new(items)
            .highlight_style(Style::default().add_modifier(Modifier::BOLD | Modifier::UNDERLINED));

        // Create fake state which is offset by scroll amount
        let mut fake_state = ListState::default()
            .with_offset(state.offset() - offset)
            .with_selected(state.selected().map(|idx| idx - offset));

        StatefulWidget::render(list, area, buf, &mut fake_state);

        // Update real state
        *state.offset_mut() = fake_state.offset() + offset;
        *state.selected_mut() = fake_state.selected().map(|idx| idx + offset);
    }
}
