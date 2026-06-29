fn main() {
    #[cfg(windows)]
    {
        let mut resource = winresource::WindowsResource::new();
        resource.set("FileDescription", "Octoglow Screensaver");
        resource.set("ProductName", "Octoglow");
        resource.set("OriginalFilename", "octoglow.scr");
        resource.set("InternalName", "octoglow");
        resource.compile().expect("compile Windows resources");
    }
}
