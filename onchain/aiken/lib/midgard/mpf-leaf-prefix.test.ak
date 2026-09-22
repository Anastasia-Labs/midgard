use aiken/merkle_patricia_forestry as mpf
use midgard/mpf_proof_v1 as proof

const before =
  #"88e28232999483e47f2779227d77d46358aad506223f4de36808d08fc57f11c4"

const after =
  #"3e090fbdb93ef5cbeb10616c4f7238e36149d89d9974620230c304e36f7a517b"

const key = #"abcd"

const value = #"01"

fn opening() -> mpf.Proof {
  [
    mpf.Leaf {
      skip: 1,
      key: #"9706e52f00c679e548b5155af5026f5af4130d7a15c990a791fff8d652c464f5",
      value: #"1111111111111111111111111111111111111111111111111111111111111111",
    },
    mpf.Branch {
      skip: 0,
      neighbors: #"2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222",
    },
  ]
}

test skipped_leaf_branch_uses_its_actual_neighbor_nibble() {
  and {
    proof.delete_root(before, key, value, opening()) == Some(after),
    proof.delete_root_paired_fold(before, key, value, opening()) == Some(after),
    proof.delete_root_two_fold(before, key, value, opening()) == Some(after),
    proof.does_not_have(after, key, opening()),
    proof.insert_root(after, key, value, opening()) == Some(before),
    proof.insert_root_paired_fold(after, key, value, opening()) == Some(before),
    proof.insert_root_two_fold(after, key, value, opening()) == Some(before),
  }
}
