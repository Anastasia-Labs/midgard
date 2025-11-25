// 3.7.3
// Payout spending validator
// The spending validator of payout addr is responsible for collecting funds from the Midgard reserve
// until a complete payout can be sent to its destination.

// Collect Reserve Funds. Collect funds from the Midgard reserve. Conditions:
// 1. Let payout input be the transaction input being spent. Let payout datum be its datum.
// 2. Let reserve input be the sole transaction input from reserve addr.
// 3. Let payout output be a transaction output.
// 4. payout input and payout output must each hold exactly one payout token and it must
// match between them.
// 5. Let value diff be the sum of values of payout input and reserve input, minus the
// value of payout.
// 6. value diff must not be negative.
// 7. If value diff is positive:
// • The transaction must send a change output to reserve addr with a value that
// matches or exceeds value diff.
// 8. The transaction must not mint or burn any tokens.

// Complete Payout. When the payout accumulator contains sufficient funds, complete the payout
// to the destination address and datum. Conditions:
// 1. Let payout input be the transaction input being spent. Let payout datum be its datum.
// 2. payout input must hold exactly one token of payout, which must be the only payout
// burned in the transaction.
// 3. Let payout output be a transaction output.
// 4. payout output.address must match payout datum.l1 address.
// 5. payout output.datum must match payout datum.l1 datum.
// 6. payout output.value must match or exceed payout datum.l2 value, excluding the
// burned token.
// 7. The transaction must not mint or burn any other tokens.
validator spend {
  spend() {
    todo
  }

  else(_) {
    fail
  }
}

// 3.7.2
// Payout minting policy
// The payout minting policy ensures that only a single payout
// accumulator can be initialized or
// payout can be completed per transaction. Redeemers:
// Mint. Mint a single payout token. Conditions:
// 1. The transaction must mint exactly one token of payout.
// 2. The transaction must burn exactly one token of withdrawal.
// 3. The minted and burned tokens must match on token name.
// 4. The transaction must not mint or burn any other tokens.
// Burn. Burn a single payout token. Conditions:
// 1. The transaction must burn exactly one token of payout.
// 2. The transaction must not mint or burn any other tokens.
validator mint {
  mint() {
    todo
  }

  else(_) {
    fail
  }
}
