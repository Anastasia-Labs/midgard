use aiken/builtin
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/transaction.{InlineDatum, Input}

pub type Evidence {
  InlineEvidence { value: Data }
  StructuredEvidence { tree: EvidenceTree }
}

/// Publications are untrusted preimages. Each consumer authenticates the
/// reconstructed payload against its header or predecessor-state commitment.
pub fn resolve(evidence: Evidence, references: List<Input>) -> Data {
  when evidence is {
    InlineEvidence { value } -> value
    StructuredEvidence { tree } -> resolve_tree(tree, references)
  }
}

pub type EvidenceTree {
  Reference { index: Int }
  Constructor { tag: Int, fields: EvidenceTree }
  ListItems { items: List<EvidenceTree> }
  MapItems { items: List<Pair<EvidenceTree, EvidenceTree>> }
  BytesParts { parts: List<EvidenceTree> }
  ListParts { parts: List<EvidenceTree> }
  MapParts { parts: List<EvidenceTree> }
}

fn resolve_tree(tree: EvidenceTree, references: List<Input>) -> Data {
  when tree is {
    Reference { index } -> {
      expect index >= 0
      expect Some(input) = list.at(references, index)
      expect InlineDatum(data) = input.output.datum
      data
    }
    Constructor { tag, fields } ->
      builtin.constr_data(
        tag,
        builtin.un_list_data(resolve_tree(fields, references)),
      )
    ListItems { items } ->
      builtin.list_data(
        list.map(items, fn(item) { resolve_tree(item, references) }),
      )
    MapItems { items } ->
      builtin.map_data(
        list.map(
          items,
          fn(pair) {
            Pair(
              resolve_tree(pair.1st, references),
              resolve_tree(pair.2nd, references),
            )
          },
        ),
      )
    BytesParts { parts } -> {
      let bytes = list.foldl(
        parts,
        #"",
        fn(part, acc) {
          bytearray.concat(
            acc,
            builtin.un_b_data(resolve_tree(part, references)),
          )
        },
      )
      builtin.b_data(bytes)
    }
    ListParts { parts } -> {
      let values = list.flat_map(
        parts,
        fn(part) { builtin.un_list_data(resolve_tree(part, references)) },
      )
      builtin.list_data(values)
    }
    MapParts { parts } -> {
      let pairs = list.flat_map(
        parts,
        fn(part) { builtin.un_map_data(resolve_tree(part, references)) },
      )
      builtin.map_data(pairs)
    }
  }
}
