/// The physical frontier authenticates inline items first, followed by the
/// resolved-input reference scripts in ascending canonical out-ref order.
/// Lookup visits reference scripts first. The inline count is the field-6
/// count authenticated at stage0 and carried unchanged in the work witness.
pub fn script_source_physical_index(
  inline_count: Int,
  source_count: Int,
  source_cursor: Int,
) -> Int {
  expect inline_count >= 0 && inline_count <= source_count
  expect source_cursor >= 0 && source_cursor < source_count
  let reference_count = source_count - inline_count
  if source_cursor < reference_count {
    inline_count + source_cursor
  } else {
    source_cursor - reference_count
  }
}
