use aiken/merkle_patricia_forestry as mpf
use midgard/mpf_proof_v1 as proof

const before =
  #"fd518b514bd5dc0c85159d10ceecac77613825d793b5025290291b458fcbd41f"

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
    proof.delete_root_paired_fold(before, key, value, opening(), #"") == Some(
      after,
    ),
    proof.does_not_have(after, key, opening()),
    proof.insert_root(after, key, value, opening()) == Some(before),
    proof.insert_root_paired_fold(after, key, value, opening()) == Some(before),
    proof.insert_root_two_fold(after, key, value, opening()) == Some(before),
  }
}
