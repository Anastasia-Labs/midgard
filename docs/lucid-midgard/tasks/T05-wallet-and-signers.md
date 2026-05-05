# T05 Wallet and Signers

## Scope

Implement wallet and signer adapters that produce vkey witnesses over
Midgard-native body hashes.

## Dependencies

- T03 core types.
- T02 native codec surface.

## Deliverables

- `MidgardWallet` interface.
- Seed phrase wallet adapter.
- Private key wallet adapter.
- External signer adapter.
- Witness verification helper.
- Wallet network/address validation.

## Acceptance Criteria

- Every produced witness verifies against the provided body hash.
- Signing never uses Cardano transaction body hash semantics.
- Address network mismatches fail before completion or signing.
- Private keys and seed phrases are never logged.

## Required Tests

- Seed phrase derives expected address and key hash.
- Private key signs body hash.
- Invalid signature is rejected.
- Cardano-domain witness does not pass Midgard body-hash verification.

## Non-Goals

- Browser wallet integration.
- Multisig policy automation.
