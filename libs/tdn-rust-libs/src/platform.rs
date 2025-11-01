use core::{
    alloc::{GlobalAlloc, Layout},
    ffi::c_char,
    panic::PanicInfo,
    sync::atomic::{AtomicBool, Ordering},
};

use alloc::string::ToString;

unsafe extern "C" {
    fn tdn_host_mallocz(size: usize, align: usize) -> *mut u8;
    fn tdn_host_free(ptr: *mut u8);
    fn tdn_host_realloc(ptr: *mut u8, new_size: usize) -> *mut u8;
    fn tdn_rust_panic(message: *const c_char, message_len: usize) -> !;
}

struct Allocator;

unsafe impl GlobalAlloc for Allocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        unsafe { tdn_host_mallocz(layout.size(), layout.align()) }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, _layout: Layout) {
        unsafe { tdn_host_free(ptr) }
    }

    unsafe fn realloc(&self, ptr: *mut u8, _layout: Layout, new_size: usize) -> *mut u8 {
        unsafe { tdn_host_realloc(ptr, new_size) }
    }
}

#[global_allocator]
static ALLOC: Allocator = Allocator;

static PANICKING: AtomicBool = AtomicBool::new(false);

fn do_panic(message: &str) -> ! {
    unsafe {
        tdn_rust_panic(message.as_ptr() as *const c_char, message.len());
    }
}

#[panic_handler]
fn handle_panic(info: &PanicInfo<'_>) -> ! {
    if PANICKING.swap(true, Ordering::Relaxed) {
        do_panic("nested panic");
    }

    do_panic(&info.to_string());
}