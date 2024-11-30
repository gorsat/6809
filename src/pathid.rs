use std::path::PathBuf;

static mut FILE_LIST: Vec<PathBuf> = Vec::new();
pub fn create_id_for_path(path: &PathBuf) -> usize {
    unsafe {
        get_id_from_path(path).unwrap_or_else(|| {
            FILE_LIST.push(path.to_owned());
            FILE_LIST.len() - 1
        })
    }
}
pub fn get_id_from_path(path: &PathBuf) -> Option<usize> { unsafe { FILE_LIST.iter().position(|f| f.eq(path)) } }
pub fn get_path_from_id(id: usize) -> Option<PathBuf> { unsafe { FILE_LIST.get(id).map(|p| p.to_owned()) } }
// pub fn get_path_display_from_id(id: usize) -> String {
//     get_path_from_id(id).map_or_else(|| red!("<PATH NOT FOUND>").to_string(), |p| p.display().to_string())
// }
pub fn get_filename_display_from_id(id: usize) -> String {
    get_path_from_id(id).map_or_else(
        || red!("<NO_PATH>").to_string(),
        |p| {
            p.file_name()
                .map_or("<NO_FILE_NAME>".to_string(), |f| f.to_string_lossy().to_string())
        },
    )
}
