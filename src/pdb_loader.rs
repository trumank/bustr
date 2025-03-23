use anyhow::Result;
use pdb::{PDB, Source};
use std::{ffi::c_void, fmt, fs::File};

#[cfg(not(target_os = "linux"))]
pub type PDBSrc = PDB<'static, File>;
#[cfg(not(target_os = "linux"))]
pub fn load(file: File) -> Result<PDBSrc> {
    Ok(PDB::open(file)?)
}

#[cfg(target_os = "linux")]
pub type PDBSrc = PDB<'static, MemmapSource>;
#[cfg(target_os = "linux")]
pub fn load(file: File) -> Result<PDBSrc> {
    Ok(PDB::open(MemmapSource::new(file))?)
}

pub struct MemmapSource {
    file: File,
}
impl MemmapSource {
    pub fn new(file: File) -> Self {
        Self { file }
    }
}
struct MemmapSourceView {
    data: &'static [u8],
}
impl std::fmt::Debug for MemmapSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MemmapSource").finish_non_exhaustive()
    }
}
impl std::fmt::Debug for MemmapSourceView {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("MemmapSourceView").finish_non_exhaustive()
    }
}
impl pdb::SourceView<'_> for MemmapSourceView {
    fn as_slice(&self) -> &[u8] {
        self.data
    }
}

impl<'s> pdb::Source<'s> for MemmapSource {
    fn view(
        &mut self,
        slices: &[pdb::SourceSlice],
    ) -> Result<Box<dyn pdb::SourceView<'s>>, std::io::Error> {
        let v = MemmapSourceView {
            data: mmap_regions_continuously(&self.file, slices),
        };

        Ok(Box::new(v))
    }
}

const PAGE_SIZE: usize = 4096;
pub fn mmap_regions_continuously(
    file: &std::fs::File,
    regions: &[pdb::SourceSlice],
) -> &'static [u8] {
    // Get the page size for alignment checking

    let mut total_size = 0;
    if let Some((last, first)) = regions.split_last() {
        for region in first {
            assert_eq!(region.size % PAGE_SIZE, 0);
            total_size += region.size;
        }
        total_size += last.size;
    }

    if total_size == 0 {
        panic!("no regions");
    }

    use std::os::unix::io::AsRawFd;
    let fd = file.as_raw_fd();

    let metadata = file.metadata().unwrap();
    let file_size = metadata.len() as usize;

    let base_ptr = unsafe {
        libc::mmap(
            std::ptr::null_mut(),
            total_size,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
            -1, // No file descriptor for anonymous mapping
            0,  // No offset for anonymous mapping
        )
    };

    if base_ptr == libc::MAP_FAILED {
        panic!("map_failed");
        //return Err(io::Error::last_os_error());
    }

    let mut mappings = Vec::with_capacity(regions.len());
    let mut current_offset = 0;

    // Now map each region into the reserved space
    for region in regions {
        // Check if region is page-aligned
        if region.offset % PAGE_SIZE as u64 != 0 {
            // Clean up previous mappings before returning error
            for (ptr, len) in &mappings {
                unsafe {
                    libc::munmap(*ptr as *mut c_void, *len);
                }
            }
            unsafe {
                libc::munmap(base_ptr, total_size);
            }

            panic!(
                "Region offset {} is not page-aligned (page size: {})",
                region.offset, PAGE_SIZE
            );
        }

        // Check if region is within file bounds
        if region.offset as usize + region.size > file_size {
            // Clean up previous mappings before returning error
            for (ptr, len) in &mappings {
                unsafe {
                    libc::munmap(*ptr as *mut c_void, *len);
                }
            }
            unsafe {
                libc::munmap(base_ptr, total_size);
            }

            panic!(
                "Region exceeds file size: offset {} + length {} > file size {}",
                region.offset, region.size, file_size
            );
        }

        // Calculate target address within our reserved space
        let target_addr = (base_ptr as usize + current_offset) as *mut c_void;

        // Map this region from the file directly into our reserved space
        // using MAP_FIXED to place it at the exact address we want
        let map_result = unsafe {
            libc::mmap(
                target_addr,
                region.size,
                libc::PROT_READ,
                libc::MAP_SHARED | libc::MAP_FIXED,
                fd,
                region.offset as i64,
            )
        };

        if map_result == libc::MAP_FAILED {
            let err = std::io::Error::last_os_error();

            // Clean up previous mappings before returning error
            for (ptr, len) in &mappings {
                unsafe {
                    libc::munmap(*ptr as *mut c_void, *len);
                }
            }
            unsafe {
                libc::munmap(base_ptr, total_size);
            }

            panic!("{err}")
        }

        // Keep track of this mapping
        mappings.push((map_result as *mut u8, region.size));

        // Move to the next position in our continuous space
        current_offset += region.size;
    }

    unsafe { std::slice::from_raw_parts(base_ptr as *mut u8, total_size) }
}
