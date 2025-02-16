use midgard/common/types.{HeaderHash}
use midgard/ledger_state.{Header}

pub type StateQueueDatum {
  key: Option<HeaderHash>,
  next: Option<HeaderHash>,
  data: Header,
}
