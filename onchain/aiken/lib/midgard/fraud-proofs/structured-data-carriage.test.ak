use aiken/builtin
use aiken/cbor
use cardano/address
use cardano/assets
use cardano/transaction.{InlineDatum, Input, Output, OutputReference}
use midgard/fraud_proofs/structured_data_carriage as carriage

fn reference(value: Data) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: #"0101010101010101010101010101010101010101010101010101010101010101",
      output_index: 0,
    },
    output: Output {
      address: address.from_script(
        #"01010101010101010101010101010101010101010101010101010101",
      ),
      value: assets.zero,
      datum: InlineDatum(value),
      reference_script: None,
    },
  }
}

fn fixture() -> carriage.Evidence {
  carriage.StructuredEvidence {
    tree: carriage.Constructor {
      tag: 7,
      fields: carriage.ListItems {
        items: [
          carriage.BytesParts {
            parts: [
              carriage.Reference { index: 0 },
              carriage.Reference { index: 1 },
            ],
          },
          carriage.ListParts {
            parts: [
              carriage.Reference { index: 2 },
              carriage.ListItems { items: [carriage.Reference { index: 3 }] },
            ],
          },
          carriage.MapParts {
            parts: [
              carriage.Reference { index: 4 },
              carriage.MapItems {
                items: [
                  Pair(
                    carriage.Reference { index: 1 },
                    carriage.Reference { index: 3 },
                  ),
                ],
              },
            ],
          },
        ],
      },
    },
  }
}

fn refs() -> List<Input> {
  [
    reference(#"aa"),
    reference(#"bb"),
    reference([1, 2]),
    reference(3),
    reference(
      builtin.map_data([Pair(builtin.b_data(#"aa"), builtin.i_data(1))]),
    ),
  ]
}

test reconstructs_nested_data_with_exact_list_map_and_byte_order() {
  let expected = builtin.constr_data(
    7,
    [
      builtin.b_data(#"aabb"),
      builtin.list_data(
        [builtin.i_data(1), builtin.i_data(2), builtin.i_data(3)],
      ),
      builtin.map_data(
        [
          Pair(builtin.b_data(#"aa"), builtin.i_data(1)),
          Pair(builtin.b_data(#"bb"), builtin.i_data(3)),
        ],
      ),
    ],
  )
  carriage.resolve(fixture(), refs()) == expected
}

test refuses_missing_publication() fail {
  carriage.resolve(fixture(), []) == builtin.i_data(0)
}

test refuses_negative_reference_index() fail {
  carriage.resolve(
    carriage.StructuredEvidence { tree: carriage.Reference { index: -1 } },
    refs(),
  ) == builtin.i_data(0)
}

test refuses_reordered_or_substituted_publication_against_committed_data() fail {
  let actual = carriage.resolve(
    fixture(),
    [
      reference(#"bb"),
      reference(#"aa"),
      reference([1, 2]),
      reference(3),
      reference(
        builtin.map_data([Pair(builtin.b_data(#"aa"), builtin.i_data(1))]),
      ),
    ],
  )
  expect actual == carriage.resolve(fixture(), refs())
  True
}

/// Shared golden emitted by structuredDataTreeData, not an Aiken-only fixture.
test typescript_map_tree_wire_decodes_and_reconstructs() {
  expect Some(data) = cbor.deserialise(#"d87c9fbfd8799f00ffd8799f01ffffff")
  expect tree: carriage.EvidenceTree = data
  carriage.resolve(carriage.StructuredEvidence { tree }, refs()) == builtin.map_data(
    [Pair(builtin.b_data(#"aa"), builtin.b_data(#"bb"))],
  )
}
