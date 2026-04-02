use aiken/builtin
use aiken/collection/dict.{Dict, has_key}
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/assets.{
  AssetName, PolicyId, Value, flatten, quantity_of, to_dict,
  without_lovelace,
}
use cardano/transaction.{InlineDatum, Input, Output}

pub const node_prefix: ByteArray = "Node"

pub type NodeDatum {
  key: NodeKey,
  link: NodeKey,
  data: Data,
}

pub type NodeKey {
  Key { key: ByteArray }
  Empty
}

pub fn get_raw_key_and_link(node_output: Output) -> (Data, Data) {
  expect InlineDatum(node_output_datum) = node_output.datum
  let constr = builtin.un_constr_data(node_output_datum).2nd
  let key = builtin.head_list(constr)
  let link = builtin.head_list(builtin.tail_list(constr))
  (key, link)
}

pub fn get_key(raw_key_and_link: (Data, Data)) -> NodeKey {
  expect key: NodeKey = raw_key_and_link.1st
  key
}

pub fn get_link(raw_key_and_link: (Data, Data)) -> NodeKey {
  expect link: NodeKey = raw_key_and_link.2nd
  link
}

pub fn get_key_and_link(node_output: Output) -> (NodeKey, NodeKey) {
  let raw = get_raw_key_and_link(node_output)
  (get_key(raw), get_link(raw))
}

pub fn serialize_key(key: NodeKey) -> ByteArray {
  when key is {
    Empty -> node_prefix
    Key { key: actual_key } -> bytearray.concat(node_prefix, actual_key)
  }
}

pub fn get_node_outputs(
  outputs: List<Output>,
  policy_id: PolicyId,
) -> List<Output> {
  outputs
    |> list.filter(fn(output) {
        let Output { value, .. } = output
        has_data_cs(policy_id, value)
      })
}

pub fn get_node_inputs(inputs: List<Input>, policy_id: PolicyId) -> List<Output> {
  filter_map_pre(
    inputs,
    fn(output) {
      let Output { value, .. } = output
      has_data_cs(policy_id, value)
    },
    fn(input) {
      let Input { output, .. } = input
      output
    },
  )
}

pub fn prove_is_root_node(policy_id: PolicyId, node: Output) -> Bool {
  expect InlineDatum(node_datum_raw) = node.datum
  expect node_datum: NodeDatum = node_datum_raw
  expect is_root_node(node_datum.key)
  expect quantity_of(node.value, policy_id, node_prefix) == 1
  True
}

pub fn prove_is_last_node(policy_id: PolicyId, node: Output) -> Bool {
  expect InlineDatum(node_datum_raw) = node.datum
  expect node_datum: NodeDatum = node_datum_raw
  expect is_last_node(node_datum.link)
  expect quantity_of(node.value, policy_id, serialize_key(node_datum.key)) == 1
  True
}

pub fn prove_is_empty_list(policy_id: PolicyId, node: Output) -> Bool {
  expect InlineDatum(node_datum_raw) = node.datum
  expect node_datum: NodeDatum = node_datum_raw
  expect is_empty_list(node_datum.key, node_datum.link)
  expect quantity_of(node.value, policy_id, node_prefix) == 1
  True
}

pub fn prove_is_member(policy_id: PolicyId, key: NodeKey, node: Output) -> Bool {
  expect InlineDatum(node_datum_raw) = node.datum
  expect node_datum: NodeDatum = node_datum_raw
  expect is_member(key, node_datum.key)
  expect quantity_of(node.value, policy_id, serialize_key(key)) == 1
  True
}

pub fn prove_is_not_member(
  policy_id: PolicyId,
  key: NodeKey,
  node: Output,
) -> Bool {
  expect InlineDatum(node_datum_raw) = node.datum
  expect node_datum: NodeDatum = node_datum_raw
  expect is_not_member(key, node_datum.key, node_datum.link)
  expect quantity_of(node.value, policy_id, serialize_key(node_datum.key)) == 1
  True
}

pub fn list_state_transition(node_mint: Dict<AssetName, Int>) -> Bool {
  dict.is_empty(node_mint) == False
}

pub fn modify_data(
  node_mint: Dict<AssetName, Int>,
  own_input: Output,
  own_output: Output,
  node_nft_policy_id: ByteArray,
  node_nft_asset_name: ByteArray,
) -> Bool {
  expect dict.is_empty(node_mint)
  expect quantity_of(own_input.value, node_nft_policy_id, node_nft_asset_name) == 1
  expect same_non_lovelace_value(own_input.value, own_output.value)
  let input_key_and_link = get_raw_key_and_link(own_input)
  let output_key_and_link = get_raw_key_and_link(own_output)
  expect input_key_and_link == output_key_and_link
  True
}

pub fn init(
  node_outputs: List<Output>,
  node_mint: Dict<AssetName, Int>,
  node_cs: ByteArray,
) -> Bool {
  expect key_added(Empty, node_mint)
  expect Some(root_node) = list.head(node_outputs)
  let raw_key_and_link = get_raw_key_and_link(root_node)
  expect is_empty_list(get_key(raw_key_and_link), get_link(raw_key_and_link))
  expect quantity_of(root_node.value, node_cs, node_prefix) == 1
  expect list.length(flatten(root_node.value)) == 2
  True
}

pub fn deinit(
  node_inputs: List<Output>,
  node_mint: Dict<AssetName, Int>,
  node_cs: ByteArray,
) -> Bool {
  expect key_removed(Empty, node_mint)
  expect Some(root_node) = list.head(node_inputs)
  let raw_key_and_link = get_raw_key_and_link(root_node)
  expect is_empty_list(get_key(raw_key_and_link), get_link(raw_key_and_link))
  expect quantity_of(root_node.value, node_cs, node_prefix) == 1
  True
}

pub fn prepend(
  key_to_prepend: NodeKey,
  prepended_node_index: Int,
  anchor_node_output_index: Int,
  node_inputs: List<Output>,
  node_outputs: List<Output>,
  node_mint: Dict<AssetName, Int>,
) -> Bool {
  expect key_added(key_to_prepend, node_mint)
  expect list.length(node_inputs) == 1
  expect Some(anchor_node_input) = list.head(node_inputs)
  expect InlineDatum(anchor_node_input_datum) = anchor_node_input.datum
  expect parsed_anchor_node_input_datum: NodeDatum = anchor_node_input_datum
  expect is_root_node(parsed_anchor_node_input_datum.key)
  expect list.length(node_outputs) == 2
  expect Some(prepended_node) = list.at(node_outputs, prepended_node_index)
  let prepended_node_raw_key_and_link = get_raw_key_and_link(prepended_node)
  expect Some(anchor_node_output) = list.at(node_outputs, anchor_node_output_index)
  expect InlineDatum(anchor_node_output_datum) = anchor_node_output.datum
  expect parsed_anchor_node_output_datum: NodeDatum = anchor_node_output_datum
  expect is_member(key_to_prepend, get_key(prepended_node_raw_key_and_link))
  expect parsed_anchor_node_input_datum.link == get_link(prepended_node_raw_key_and_link)
  let anchor_node_new_datum =
    NodeDatum {
      key: parsed_anchor_node_input_datum.key,
      link: key_to_prepend,
      data: parsed_anchor_node_input_datum.data,
    }
  expect list.length(flatten(prepended_node.value)) == 2
  expect anchor_node_input.address == anchor_node_output.address
  expect same_non_lovelace_value(anchor_node_input.value, anchor_node_output.value)
  expect anchor_node_new_datum == parsed_anchor_node_output_datum
  expect prepended_node.address == anchor_node_output.address
  expect
    is_not_member(
      key_to_prepend,
      node_key: parsed_anchor_node_input_datum.key,
      node_link: parsed_anchor_node_input_datum.link,
    )
  True
}

pub fn append(
  key_to_append: NodeKey,
  appended_node_index: Int,
  anchor_node_output_index: Int,
  node_inputs: List<Output>,
  node_outputs: List<Output>,
  node_mint: Dict<AssetName, Int>,
) -> Bool {
  expect key_added(key_to_append, node_mint)
  expect list.length(node_inputs) == 1
  expect Some(anchor_node_input) = list.head(node_inputs)
  expect InlineDatum(anchor_node_input_datum) = anchor_node_input.datum
  expect parsed_anchor_node_input_datum: NodeDatum = anchor_node_input_datum
  let anchor_node_raw_key_and_link = get_raw_key_and_link(anchor_node_input)
  expect is_last_node(get_link(anchor_node_raw_key_and_link))
  expect list.length(node_outputs) == 2
  expect Some(appended_node) = list.at(node_outputs, appended_node_index)
  let appended_node_raw_key_and_link = get_raw_key_and_link(appended_node)
  expect Some(anchor_node_output) = list.at(node_outputs, anchor_node_output_index)
  expect InlineDatum(anchor_node_output_datum) = anchor_node_output.datum
  expect parsed_anchor_node_output_datum: NodeDatum = anchor_node_output_datum
  expect is_member(key_to_append, get_key(appended_node_raw_key_and_link))
  expect is_last_node(get_link(appended_node_raw_key_and_link))
  let anchor_node_new_datum =
    NodeDatum {
      key: parsed_anchor_node_input_datum.key,
      link: key_to_append,
      data: parsed_anchor_node_input_datum.data,
    }
  expect list.length(flatten(appended_node.value)) == 2
  expect anchor_node_input.address == anchor_node_output.address
  expect same_non_lovelace_value(anchor_node_input.value, anchor_node_output.value)
  expect anchor_node_new_datum == parsed_anchor_node_output_datum
  expect appended_node.address == anchor_node_output.address
  expect
    is_not_member(
      key_to_append,
      node_key: parsed_anchor_node_input_datum.key,
      node_link: parsed_anchor_node_input_datum.link,
    )
  True
}

pub fn insert(
  key_to_insert: NodeKey,
  inserted_node_output_index: Int,
  anchor_node_output_index: Int,
  node_inputs: List<Output>,
  node_outputs: List<Output>,
  node_mint: Dict<AssetName, Int>,
) -> Bool {
  expect key_added(key_to_insert, node_mint)
  expect list.length(node_inputs) == 1
  expect Some(anchor_node_input) = list.head(node_inputs)
  expect InlineDatum(anchor_node_input_datum) = anchor_node_input.datum
  expect parsed_anchor_node_input_datum: NodeDatum = anchor_node_input_datum
  expect is_not_member(
    key_to_insert,
    parsed_anchor_node_input_datum.key,
    parsed_anchor_node_input_datum.link,
  )
  expect list.length(node_outputs) == 2
  expect Some(inserted_node_output) = list.at(node_outputs, inserted_node_output_index)
  let inserted_node_output_raw_key_and_link = get_raw_key_and_link(inserted_node_output)
  expect Some(anchor_node_output) = list.at(node_outputs, anchor_node_output_index)
  expect InlineDatum(anchor_node_output_datum) = anchor_node_output.datum
  expect parsed_anchor_node_output_datum: NodeDatum = anchor_node_output_datum
  expect is_member(key_to_insert, get_key(inserted_node_output_raw_key_and_link))
  let anchor_node_new_datum =
    NodeDatum {
      key: parsed_anchor_node_input_datum.key,
      link: key_to_insert,
      data: parsed_anchor_node_input_datum.data,
    }
  expect parsed_anchor_node_input_datum.link == get_link(inserted_node_output_raw_key_and_link)
  expect list.length(flatten(inserted_node_output.value)) == 2
  expect anchor_node_input.address == anchor_node_output.address
  expect same_non_lovelace_value(anchor_node_input.value, anchor_node_output.value)
  expect anchor_node_new_datum == parsed_anchor_node_output_datum
  expect inserted_node_output.address == anchor_node_output.address
  True
}

pub fn remove(
  key_to_remove: NodeKey,
  removed_node_input_index: Int,
  anchor_node_input_index: Int,
  node_inputs: List<Output>,
  node_outputs: List<Output>,
  node_mint: Dict<AssetName, Int>,
) -> Bool {
  expect key_removed(key_to_remove, node_mint)
  expect list.length(node_inputs) == 2
  expect Some(removed_node_input) = list.at(node_inputs, removed_node_input_index)
  let removed_node_raw_key_and_link = get_raw_key_and_link(removed_node_input)
  expect Some(anchor_node_input) = list.at(node_inputs, anchor_node_input_index)
  expect InlineDatum(anchor_node_input_datum) = anchor_node_input.datum
  expect parsed_anchor_node_input_datum: NodeDatum = anchor_node_input_datum
  expect is_member(key_to_remove, get_key(removed_node_raw_key_and_link))
  expect parsed_anchor_node_input_datum.link == key_to_remove
  expect list.length(node_outputs) == 1
  expect Some(anchor_node_output) = list.head(node_outputs)
  expect InlineDatum(anchor_node_output_datum) = anchor_node_output.datum
  expect parsed_anchor_node_output_datum: NodeDatum = anchor_node_output_datum
  let anchor_node_new_datum =
    NodeDatum {
      key: parsed_anchor_node_input_datum.key,
      link: get_link(removed_node_raw_key_and_link),
      data: parsed_anchor_node_input_datum.data,
    }
  expect anchor_node_input.address == anchor_node_output.address
  expect same_non_lovelace_value(anchor_node_input.value, anchor_node_output.value)
  expect anchor_node_new_datum == parsed_anchor_node_output_datum
  True
}

fn filter_map_pre(
  self: List<a>,
  predicate: fn(b) -> Bool,
  transform: fn(a) -> b,
) -> List<b> {
  when self is {
    [] -> []
    [x, ..xs] -> {
      let y = transform(x)
      if predicate(y) {
        [y, ..filter_map_pre(xs, predicate, transform)]
      } else {
        filter_map_pre(xs, predicate, transform)
      }
    }
  }
}

fn has_data_cs(cs: PolicyId, value: Value) -> Bool {
  value
    |> to_dict()
    |> has_key(cs)
}

fn is_root_node(node_key: NodeKey) -> Bool {
  node_key == Empty
}

fn is_last_node(node_link: NodeKey) -> Bool {
  node_link == Empty
}

fn is_empty_list(node_key: NodeKey, node_link: NodeKey) -> Bool {
  and {
    is_root_node(node_key),
    is_last_node(node_link),
  }
}

fn key_added(key: NodeKey, node_mint: Dict<AssetName, Int>) -> Bool {
  and {
    dict.size(node_mint) == 1,
    dict.get(node_mint, serialize_key(key)) == Some(1),
  }
}

fn key_removed(key: NodeKey, node_mint: Dict<AssetName, Int>) -> Bool {
  and {
    dict.size(node_mint) == 1,
    dict.get(node_mint, serialize_key(key)) == Some(-1),
  }
}

fn is_member(k: NodeKey, node_key: NodeKey) -> Bool {
  k == node_key
}

fn is_not_member(k: NodeKey, node_key: NodeKey, node_link: NodeKey) -> Bool {
  expect Key { key: actual_k } = k
  and {
    when node_key is {
      Empty -> True
      Key { key: actual_node_key } ->
        bytearray.compare(actual_node_key, actual_k) == Less
    },
    when node_link is {
      Empty -> True
      Key { key: actual_node_link } ->
        bytearray.compare(actual_k, actual_node_link) == Less
    },
  }
}

fn same_non_lovelace_value(left: Value, right: Value) -> Bool {
  without_lovelace(left) == without_lovelace(right)
}
