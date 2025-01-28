use midgard/common/types.{Header, HeaderHash}

pub type StateQueueDatum {
  key: Option<HeaderHash>,
  next: Option<HeaderHash>,
  data: Header,
}
