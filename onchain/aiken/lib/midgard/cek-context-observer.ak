use aiken/primitive/bytearray
use midgard/cek_context_chain as chain
use midgard/validation_machine/machine_types.{CekContextControlV1}
use midgard/validation_machine/shared.{
  finalize_cek_observer_items_v1, max_tx_size_derived_collection_item_count,
  prepend_cek_observer_item_v1,
}
use midgard/validation_machine/witness.{observer_context_is_pristine}

pub type ObserverOpening {
  EmptyObservers
  FinishedObservers
  ObserverItem { count: Int, hash: ByteArray }
}

pub type ObserverPending {
  staged: Data,
  opening: ObserverOpening,
}

pub fn next_observer(
  staged: chain.ContextStaged,
  opening: ObserverOpening,
) -> chain.ContextSuccessor {
  let context = staged.context
  let midgard = context.language_tag == 128
  let next = when opening is {
    EmptyObservers -> {
      expect observer_context_is_pristine(context)
      CekContextControlV1 {
        ..context,
        stage: 6,
        observer_summary: finalize_cek_observer_items_v1(
          context.observer_items,
          midgard,
        ),
      }
    }
    FinishedObservers -> {
      expect
        context.observer_count > 0 && context.observer_items.length == context.observer_count
      CekContextControlV1 {
        ..context,
        stage: 6,
        observer_summary: finalize_cek_observer_items_v1(
          context.observer_items,
          midgard,
        ),
      }
    }
    ObserverItem { count, hash } -> {
      expect count > 0 && count <= max_tx_size_derived_collection_item_count
      expect bytearray.length(hash) == 28
      expect
        if context.observer_items.length == 0 {
          context.previous_observer == #""
        } else {
          bytearray.compare(hash, context.previous_observer) == Less
        }
      CekContextControlV1 {
        ..context,
        observer_count: count,
        observer_items: prepend_cek_observer_item_v1(
          hash,
          midgard,
          context.observer_items,
        ),
        previous_observer: hash,
      }
    }
  }
  chain.ContinueContext { next }
}
