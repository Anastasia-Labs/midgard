//// Node-agreement vectors for the withdrawal signature rule.
////
//// Each test judges a withdrawal signature with step-03's
//// `withdrawal_signature_is_valid_v1` and pins the verdict. The TypeScript
//// twin, demo/midgard-sdk/tests/withdrawal-signature.ledger-exact.test.ts,
//// signs the same bodies with the same key, pins these signatures, and checks
//// the SDK's `verifyWithdrawalSignatureCbor` reaches the same verdict. The
//// datums nest 1,000 deep, within this test runner's own stack.

use aiken/builtin
use aiken/crypto
use cardano/address
use cardano/transaction.{InlineDatum, OutputReference}
use midgard/fraud_proofs/withdrawal_mistag/step_03
use midgard/ledger_state

const public_key: ByteArray =
  #"ea4a6c63e29c520abef5507b132ec5f9954776aebebe7b92421eea691446d22c"

const owner: ByteArray =
  #"8b218424ad74df25d35c2ea8e094a4c5c5aeb2cbb442419331569313"

/// A list nested `depth` deep around the integer 0.
fn nested_lists(depth: Int) -> Data {
  if depth == 0 {
    builtin.i_data(0)
  } else {
    builtin.list_data([nested_lists(depth - 1)])
  }
}

/// A constructor 0 nested `depth` deep around the integer 0.
fn nested_constructors(depth: Int) -> Data {
  if depth == 0 {
    builtin.i_data(0)
  } else {
    builtin.constr_data(0, [nested_constructors(depth - 1)])
  }
}

fn chain_valid(
  datum: Data,
  body_owner: ByteArray,
  key: ByteArray,
  signature: ByteArray,
) -> Bool {
  step_03.withdrawal_signature_is_valid_v1(
    ledger_state.WithdrawalInfo {
      body: ledger_state.WithdrawalBody {
        l2_outref: OutputReference {
          transaction_id: #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
          output_index: 0,
        },
        l2_owner: body_owner,
        l2_value: [Pair(#"", [Pair(#"", 20000000)])],
        l1_address: address.from_verification_key(owner),
        l1_datum: InlineDatum(datum),
      },
      signature: Pair(key, signature),
      validity: ledger_state.WithdrawalIsValid,
    },
  )
}

test agrees_honest_signature_over_nested_lists() {
  chain_valid(
    nested_lists(1000),
    owner,
    public_key,
    #"66bed881e2eb231e4c41e57c45e45489d08246250a2660a39d91cebb0e455631331fcfbe9194863326126a803566a0af02131cc6f2d6aeffb6de5d2370a52305",
  ) == True
}

test agrees_other_body_signature_over_nested_lists() {
  chain_valid(
    nested_lists(1000),
    owner,
    public_key,
    #"5568dcfe380b2b085f0c21c4ba1e25ac80c28e0f4d360413cf2542f51b2baa0ad88bdf30ab4759373b57cfa7f890f102a7fd6037f97c06dbd035d4014f7ff909",
  ) == False
}

test agrees_honest_signature_over_nested_constructors() {
  chain_valid(
    nested_constructors(1000),
    owner,
    public_key,
    #"afd0ab5bd0f13d82c6903130ae1e57e14f883585d145cdb62edc12c892aaecb5f89696b055a25961a65574ed9a9fb78095cdfd6c71f489e28170f40bcae62103",
  ) == True
}

test agrees_other_body_signature_over_nested_constructors() {
  chain_valid(
    nested_constructors(1000),
    owner,
    public_key,
    #"3c19b1693e2bfe66c194197cfd41909da60fed3956d600a0a805b87ac1f00f90cc39cbf2013361a2ba35aff8d2a4da11c0722c77ea93f0e7597f622ed266ec09",
  ) == False
}

test agrees_key_with_y_above_the_field_prime() {
  let key = #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f"
  chain_valid(
    builtin.i_data(0),
    crypto.blake2b_224(key),
    key,
    #"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
  ) == False
}

test agrees_key_with_no_curve_point() {
  let key = #"0200000000000000000000000000000000000000000000000000000000000000"
  chain_valid(
    builtin.i_data(0),
    crypto.blake2b_224(key),
    key,
    #"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
  ) == False
}
