use aiken/builtin
use midgard/cek_constant_v1
use midgard/cek_machine_v1 as machine

pub fn decode_compute(raw: Data) -> machine.CoreStepWitnessV1 {
  let Pair(tag, fields) = builtin.un_constr_data(raw)
  when tag is {
    0 -> {
      expect [index_data] = fields
      expect index: Int = index_data
      machine.ComputeVariable { index }
    }
    1 -> {
      expect [value_data] = fields
      expect value: cek_constant_v1.ConstantWitnessV1 = value_data
      machine.ComputeConstant { value }
    }
    2 -> {
      expect [body_data] = fields
      expect body: ByteArray = body_data
      machine.ComputeLambda { body }
    }
    3 -> {
      expect [body_data] = fields
      expect body: ByteArray = body_data
      machine.ComputeDelay { body }
    }
    4 -> {
      expect [function_data, argument_data] = fields
      expect function: ByteArray = function_data
      expect argument: ByteArray = argument_data
      machine.ComputeApplication { function, argument }
    }
    5 -> {
      expect [term_data] = fields
      expect term: ByteArray = term_data
      machine.ComputeForce { term }
    }
    6 -> {
      expect [] = fields
      machine.ComputeError
    }
    7 -> {
      expect [tag_data] = fields
      expect tag: Int = tag_data
      machine.ComputeBuiltin { tag }
    }
    8 -> {
      expect [tag_data] = fields
      expect tag: Int = tag_data
      machine.ComputeConstrEmpty { tag }
    }
    9 -> {
      expect
          [
            tag_data, terms_count_data, first_term_data,
            remaining_terms_root_data,
          ]
        = fields
      expect tag: Int = tag_data
      expect terms_count: Int = terms_count_data
      expect first_term: ByteArray = first_term_data
      expect remaining_terms_root: ByteArray = remaining_terms_root_data
      machine.ComputeConstrNonEmpty {
        tag,
        terms_count,
        first_term,
        remaining_terms_root,
      }
    }
    10 -> {
      expect [scrutinee_data, branches_count_data, branches_root_data] = fields
      expect scrutinee: ByteArray = scrutinee_data
      expect branches_count: Int = branches_count_data
      expect branches_root: ByteArray = branches_root_data
      machine.ComputeCase { scrutinee, branches_count, branches_root }
    }
    40 -> {
      expect [value_root_data] = fields
      expect value_root: ByteArray = value_root_data
      machine.ComputeContextConstant { value_root }
    }
    _ -> fail
  }
}
