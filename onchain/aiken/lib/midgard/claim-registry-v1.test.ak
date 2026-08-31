//// Pure claim-registry mutation and maximum-proof-fit selectors.

use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/transaction.{Input}
use midgard/claim_registry_v1
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/mpf_proof_v1

const category_id = #"0000002a"

const header_hash = #"abababababababababababababababababababababababababababab"

const other_header_hash =
  #"cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd"

fn claim_id() -> ByteArray {
  bytearray.concat(category_id, header_hash)
}

fn other_claim_id() -> ByteArray {
  bytearray.concat(category_id, other_header_hash)
}

fn maximum_absence_proof() -> mpf.Proof {
  fixture.adversarial_non_membership_steps(63)
}

fn maximum_absence_root() -> ByteArray {
  fixture.adversarial_non_membership_root(claim_id(), maximum_absence_proof())
}

fn maximum_membership_proof() -> mpf.Proof {
  fixture.adversarial_branch_steps(64)
}

fn maximum_live_root() -> ByteArray {
  fixture.adversarial_membership_root(
    claim_id(),
    claim_registry_v1.live_claim_value,
    maximum_membership_proof(),
  )
}

fn published_chunks(steps: mpf.Proof) -> List<Input> {
  fixture.proof_chunk_reference_inputs(steps)
}

// ## Maximum-depth measurement selectors
//
// Each measured selector has a fixture-only twin so the caller can report both
// the whole-test reading and the mutation seam's marginal execution cost. The
// direct route is intentionally measured even though its 64 Branch/Leaf steps
// do not fit the signed transaction envelope; it proves the two carriages have
// identical semantics. The published route is the capability-preserving path.

test claim_registry_maximum_64_step_direct_open_fixture_only() {
  let steps = maximum_absence_proof()
  let root = maximum_absence_root()
  and {
    list.length(steps) == mpf_proof_v1.maximum_proof_step_count,
    bytearray.length(root) == 32,
  }
}

test claim_registry_maximum_64_step_direct_open_mutation() {
  let steps = maximum_absence_proof()
  expect Some(successor) = claim_registry_v1.successor_root(
    maximum_absence_root(),
    claim_registry_v1.OpenClaim {
      claim_id: claim_id(),
      proof: claim_registry_v1.RedeemerCarried(steps),
    },
    [],
  )
  and {
    bytearray.length(successor) == 32,
    successor != maximum_absence_root(),
  }
}

test claim_registry_maximum_64_step_published_open_fixture_only() {
  let steps = maximum_absence_proof()
  let chunks = published_chunks(steps)
  and {
    list.length(steps) == mpf_proof_v1.maximum_proof_step_count,
    list.length(chunks) == 4,
    bytearray.length(maximum_absence_root()) == 32,
  }
}

test claim_registry_maximum_64_step_published_open_mutation() {
  let steps = maximum_absence_proof()
  let chunks = published_chunks(steps)
  expect Some(successor) = claim_registry_v1.successor_root(
    maximum_absence_root(),
    claim_registry_v1.OpenClaim {
      claim_id: claim_id(),
      proof: claim_registry_v1.PublishedChunks {
        ordered_chunk_reference_input_indices: [0, 1, 2, 3],
      },
    },
    chunks,
  )
  and {
    bytearray.length(successor) == 32,
    successor != maximum_absence_root(),
  }
}

test claim_registry_maximum_64_step_published_close_fixture_only() {
  let steps = maximum_membership_proof()
  let chunks = published_chunks(steps)
  and {
    list.length(steps) == mpf_proof_v1.maximum_proof_step_count,
    list.length(chunks) == 4,
    bytearray.length(maximum_live_root()) == 32,
  }
}

test claim_registry_maximum_64_step_published_close_mutation() {
  let steps = maximum_membership_proof()
  let chunks = published_chunks(steps)
  expect Some(successor) = claim_registry_v1.successor_root(
    maximum_live_root(),
    claim_registry_v1.CloseClaim {
      claim_id: claim_id(),
      proof: claim_registry_v1.PublishedChunks {
        ordered_chunk_reference_input_indices: [0, 1, 2, 3],
      },
    },
    chunks,
  )
  and {
    bytearray.length(successor) == 32,
    successor != maximum_live_root(),
  }
}

test claim_registry_maximum_64_step_published_cancel_mutation() {
  let steps = maximum_membership_proof()
  let chunks = published_chunks(steps)
  expect Some(successor) = claim_registry_v1.successor_root(
    maximum_live_root(),
    claim_registry_v1.CancelClaim {
      claim_id: claim_id(),
      proof: claim_registry_v1.PublishedChunks {
        ordered_chunk_reference_input_indices: [0, 1, 2, 3],
      },
    },
    chunks,
  )
  and {
    bytearray.length(successor) == 32,
    successor != maximum_live_root(),
  }
}

// Kept separate from the fit rows above: this semantic control intentionally
// pays for the compatibility helper's additional proof walks so the measured
// mutation selector remains an honest bound on the production seam.
test claim_registry_maximum_64_step_open_successor_is_exact() {
  let steps = maximum_absence_proof()
  expect Some(successor) = claim_registry_v1.successor_root(
    maximum_absence_root(),
    claim_registry_v1.OpenClaim {
      claim_id: claim_id(),
      proof: claim_registry_v1.RedeemerCarried(steps),
    },
    [],
  )
  expect Some(library_successor) = mpf_proof_v1.insert_root(
    maximum_absence_root(),
    claim_id(),
    claim_registry_v1.live_claim_value,
    steps,
  )
  successor == library_successor
}

test claim_registry_maximum_64_step_cancel_successor_is_exact() {
  let steps = maximum_membership_proof()
  expect Some(successor) = claim_registry_v1.successor_root(
    maximum_live_root(),
    claim_registry_v1.CancelClaim {
      claim_id: claim_id(),
      proof: claim_registry_v1.RedeemerCarried(steps),
    },
    [],
  )
  expect Some(library_successor) = mpf_proof_v1.delete_root(
    maximum_live_root(),
    claim_id(),
    claim_registry_v1.live_claim_value,
    steps,
  )
  successor == library_successor
}

// ## Transition semantics and hostile proof carriage

test claim_registry_open_cancel_and_close_have_exact_roots() {
  let empty = mpf_proof_v1.insert_root(
    claim_registry_v1.empty_claims_root,
    claim_id(),
    claim_registry_v1.live_claim_value,
    [],
  )
  expect Some(live_root) = empty
  expect Some(closed_root) = claim_registry_v1.successor_root(
    live_root,
    claim_registry_v1.CloseClaim {
      claim_id: claim_id(),
      proof: claim_registry_v1.RedeemerCarried([]),
    },
    [],
  )
  expect Some(reopened_root) = claim_registry_v1.successor_root(
    live_root,
    claim_registry_v1.CancelClaim {
      claim_id: claim_id(),
      proof: claim_registry_v1.RedeemerCarried([]),
    },
    [],
  )
  and {
    mpf_proof_v1.has(
      live_root,
      claim_id(),
      claim_registry_v1.live_claim_value,
      [],
    ),
    mpf_proof_v1.has(
      closed_root,
      claim_id(),
      claim_registry_v1.closed_claim_value,
      [],
    ),
    reopened_root == claim_registry_v1.empty_claims_root,
  }
}

test claim_registry_rejects_duplicate_live_or_closed_open() {
  let live_root = fixture.adversarial_membership_root(
    claim_id(),
    claim_registry_v1.live_claim_value,
    [],
  )
  let closed_root = fixture.adversarial_membership_root(
    claim_id(),
    claim_registry_v1.closed_claim_value,
    [],
  )
  and {
    claim_registry_v1.successor_root(
      live_root,
      claim_registry_v1.OpenClaim {
        claim_id: claim_id(),
        proof: claim_registry_v1.RedeemerCarried([]),
      },
      [],
    ) == None,
    claim_registry_v1.successor_root(
      closed_root,
      claim_registry_v1.OpenClaim {
        claim_id: claim_id(),
        proof: claim_registry_v1.RedeemerCarried([]),
      },
      [],
    ) == None,
  }
}

test claim_registry_rejects_a_wrong_claim_or_root() {
  let steps = maximum_absence_proof()
  and {
    claim_registry_v1.successor_root(
      maximum_absence_root(),
      claim_registry_v1.OpenClaim {
        claim_id: other_claim_id(),
        proof: claim_registry_v1.RedeemerCarried(steps),
      },
      [],
    ) == None,
    claim_registry_v1.successor_root(
      blake2b_256("wrong-root"),
      claim_registry_v1.OpenClaim {
        claim_id: claim_id(),
        proof: claim_registry_v1.RedeemerCarried(steps),
      },
      [],
    ) == None,
  }
}

test claim_registry_rejects_reordered_or_duplicated_published_chunks() {
  let steps = maximum_absence_proof()
  let chunks = published_chunks(steps)
  and {
    claim_registry_v1.successor_root(
      maximum_absence_root(),
      claim_registry_v1.OpenClaim {
        claim_id: claim_id(),
        proof: claim_registry_v1.PublishedChunks {
          ordered_chunk_reference_input_indices: [0, 2, 1, 3],
        },
      },
      chunks,
    ) == None,
    claim_registry_v1.successor_root(
      maximum_absence_root(),
      claim_registry_v1.OpenClaim {
        claim_id: claim_id(),
        proof: claim_registry_v1.PublishedChunks {
          ordered_chunk_reference_input_indices: [0, 1, 1, 3],
        },
      },
      chunks,
    ) == None,
  }
}

test claim_registry_rejects_more_than_64_steps_on_both_carriages() {
  let oversized = fixture.adversarial_non_membership_steps(64)
  let chunks = published_chunks(oversized)
  and {
    list.length(oversized) == mpf_proof_v1.maximum_proof_step_count + 1,
    list.length(chunks) == 5,
    claim_registry_v1.successor_root(
      maximum_absence_root(),
      claim_registry_v1.OpenClaim {
        claim_id: claim_id(),
        proof: claim_registry_v1.RedeemerCarried(oversized),
      },
      [],
    ) == None,
    claim_registry_v1.successor_root(
      maximum_absence_root(),
      claim_registry_v1.OpenClaim {
        claim_id: claim_id(),
        proof: claim_registry_v1.PublishedChunks {
          ordered_chunk_reference_input_indices: [0, 1, 2, 3, 4],
        },
      },
      chunks,
    ) == None,
  }
}
