import { readFileSync } from "node:fs";
import { join } from "node:path";

import {
  bytes,
  rebindAikenConstants,
} from "@al-ft/midgard-core/scripts/golden-channel.mjs";

import {
  addressWitnessItems,
  addressWitnessVerificationKey,
  blobChunk,
  mint,
  observerScript,
  observerSignerHash,
  outputs,
  redeemer,
  referenceInputs,
  repositoryRoot,
  signerWitness,
  singleFieldItem,
  spendInputs,
  spendRedeemer,
  terminalFixtureConstants,
  terminalItemCount,
  writeOrCheck,
} from "./generate-ordered-collection-boundary-aiken-goldens.terminal-fixture-constants.mjs";

const AIKEN_FAMILIES = [
  {
    aiken: "onchain/aiken/lib/midgard/fraud-proofs/native-tx-v1.test.ak",
    constants: {
      cardano_max_transaction_bytes: signerWitness.cardanoMaxTransactionBytes,

      c20_7_maximum_cardano_vkey_witness_count: signerWitness.vkeyWitnessCount,
      c20_7_maximum_cardano_field_bytes: signerWitness.addressWitnessFieldBytes,
      c20_7_maximum_cardano_signed_cardano_bytes:
        signerWitness.acceptedSignedCardanoBytes,
      c20_7_adjacent_cardano_signed_cardano_bytes:
        signerWitness.adjacentSignedCardanoBytes,
      c20_7_maximum_cardano_canonical_bytes: signerWitness.nativeCanonicalBytes,
      c20_7_maximum_cardano_transaction_id: bytes(
        signerWitness.transactionIdHex,
      ),
      c20_7_maximum_cardano_transaction_commitment: bytes(
        signerWitness.transactionCommitmentHex,
      ),
      c20_7_maximum_cardano_collection_commitment: bytes(
        signerWitness.addressWitnessFieldCommitmentHex,
      ),
      c20_7_maximum_cardano_preimage_hash: bytes(
        signerWitness.addressWitnessFieldPreimageHashHex,
      ),
      c20_7_maximum_cardano_compact_cbor: bytes(signerWitness.compactCborHex),
      c20_7_maximum_cardano_witness_set_compact_cbor: bytes(
        signerWitness.witnessSetCompactCborHex,
      ),
      c20_7_maximum_cardano_field_preimage_lengths_cbor: bytes(
        signerWitness.fieldPreimageLengthsCborHex,
      ),
      c20_7_maximum_cardano_address_witnesses_preimage_cbor: bytes(
        signerWitness.addressWitnessFieldPreimageCborHex,
      ),
      c20_7_maximum_cardano_first_verification_key: bytes(
        addressWitnessVerificationKey(addressWitnessItems[0]),
      ),
      c20_7_maximum_cardano_last_verification_key: bytes(
        addressWitnessVerificationKey(addressWitnessItems.at(-1)),
      ),

      c20_6_maximum_cardano_native_script_witness_count:
        observerScript.nativeScriptWitnessCount,
      c20_6_maximum_cardano_field_bytes: observerScript.scriptWitnessFieldBytes,
      c20_6_maximum_cardano_signed_cardano_bytes:
        observerScript.acceptedSignedCardanoBytes,
      c20_6_adjacent_cardano_signed_cardano_bytes:
        observerScript.adjacentSignedCardanoBytes,
      c20_6_maximum_cardano_canonical_bytes:
        observerScript.nativeCanonicalBytes,
      c20_6_maximum_cardano_transaction_id: bytes(
        observerScript.transactionIdHex,
      ),
      c20_6_maximum_cardano_transaction_commitment: bytes(
        observerScript.transactionCommitmentHex,
      ),
      c20_6_maximum_cardano_collection_commitment: bytes(
        observerScript.scriptWitnessFieldCommitmentHex,
      ),
      c20_6_maximum_cardano_preimage_hash: bytes(
        observerScript.scriptWitnessFieldPreimageHashHex,
      ),
      c20_6_maximum_cardano_compact_cbor: bytes(observerScript.compactCborHex),
      c20_6_maximum_cardano_witness_set_compact_cbor: bytes(
        observerScript.witnessSetCompactCborHex,
      ),
      c20_6_maximum_cardano_field_preimage_lengths_cbor: bytes(
        observerScript.fieldPreimageLengthsCborHex,
      ),
      c20_6_maximum_cardano_script_witnesses_preimage_cbor: bytes(
        observerScript.scriptWitnessFieldPreimageCborHex,
      ),
      // §5.1's wrapped width of one field-6 item, so the adjacent-count test
      // states the stride once instead of carrying a literal that the envelope
      // silently invalidates.
      c20_6_field_item_stride_bytes:
        observerScript.scriptWitnessItemStrideBytes,
      c20_6_maximum_cardano_signer_hash: observerSignerHash(),
      c20_6_maximum_cardano_expiry_base: observerScript.observerExpiryBase,
    },
  },
  {
    aiken:
      "onchain/aiken/lib/midgard/fraud-proofs/native-tx.max-redeemers.test.ak",
    constants: {
      maximum_cardano_spend_redeemer_count: spendRedeemer.redeemerCount,
      maximum_cardano_spend_redeemer_preimage_bytes:
        spendRedeemer.redeemerFieldBytes,
      maximum_cardano_spend_redeemer_preimage_hash: bytes(
        spendRedeemer.redeemerFieldPreimageHashHex,
      ),
      maximum_cardano_spend_redeemer_collection_commitment: bytes(
        spendRedeemer.redeemerFieldCommitmentHex,
      ),
      maximum_cardano_spend_redeemer_cbor: redeemer.redeemerCbor,
      maximum_cardano_spend_redeemer_ex_memory: redeemer.executionMemory,
      maximum_cardano_spend_redeemer_ex_steps: redeemer.executionSteps,
      maximum_cardano_transaction_id: bytes(spendRedeemer.transactionIdHex),
      maximum_cardano_transaction_commitment: bytes(
        spendRedeemer.transactionCommitmentHex,
      ),
      maximum_cardano_compact_cbor: bytes(spendRedeemer.compactCborHex),
      maximum_cardano_witness_set_compact_cbor: bytes(
        spendRedeemer.witnessSetCompactCborHex,
      ),
      maximum_cardano_field_preimage_lengths_cbor: bytes(
        spendRedeemer.fieldPreimageLengthsCborHex,
      ),
      maximum_cardano_validation_context_cbor: bytes(
        spendRedeemer.validationContextCborHex,
      ),
      maximum_cardano_terminal_encoded_length_before_item:
        spendRedeemer.terminalEncodedLengthBeforeItem,
      maximum_cardano_terminal_pre_work_root: bytes(
        spendRedeemer.preWorkRootHex,
      ),
      maximum_cardano_terminal_post_work_root: bytes(
        spendRedeemer.postWorkRootHex,
      ),
    },
  },
  {
    aiken:
      "onchain/aiken/lib/midgard/fraud-proofs/native-tx.max-inline-datum.test.ak",
    constants: {
      maximum_cardano_inline_datum_transaction_id: bytes(
        blobChunk.transactionIdHex,
      ),
      maximum_cardano_inline_datum_transaction_commitment: bytes(
        blobChunk.transactionCommitmentHex,
      ),
      maximum_cardano_inline_datum_compact_cbor: bytes(
        blobChunk.compactCborHex,
      ),
      maximum_cardano_inline_datum_witness_set_compact_cbor: bytes(
        blobChunk.witnessSetCompactCborHex,
      ),
      maximum_cardano_inline_datum_field_preimage_lengths_cbor: bytes(
        blobChunk.fieldPreimageLengthsCborHex,
      ),
      maximum_cardano_inline_datum_validation_context_cbor: bytes(
        blobChunk.validationContextCborHex,
      ),
      maximum_cardano_inline_datum_terminal_pre_work_root: bytes(
        blobChunk.preWorkRootHex,
      ),
      maximum_cardano_inline_datum_terminal_post_work_root: bytes(
        blobChunk.postWorkRootHex,
      ),
      // #592: the one genuinely new producer value the machine rebind needed.
      // §8's carriage is the field's whole §5.1 preimage, and that module's
      // field-2 preimage is a single 16,221-byte output item it only ever built
      // the terminal 3,936-byte chunk of. §5.1's splitter is applied here so the
      // Aiken constant is the bare *item* and the envelope stays derived in
      // Aiken by `encode_field_preimage` — one published value that cannot
      // disagree with itself.
      maximum_cardano_inline_datum_item_cbor: singleFieldItem(
        blobChunk.outputsFieldPreimageCborHex,
      ),
    },
  },
  {
    aiken: "onchain/aiken/lib/midgard/validation-machine-tests/constants.ak",
    constants: {
      ...terminalFixtureConstants({
        prefix: "maximum_spend_input",
        fold: spendInputs,
        preimageHex: spendInputs.fieldPreimageCborHex,
        itemCount: terminalItemCount(
          spendInputs.fieldPreimageCborHex,
          spendInputs,
        ),
      }),
      ...terminalFixtureConstants({
        prefix: "maximum_reference_input",
        fold: referenceInputs,
        preimageHex: referenceInputs.fieldPreimageCborHex,
        itemCount: terminalItemCount(
          referenceInputs.fieldPreimageCborHex,
          referenceInputs,
        ),
      }),
      ...terminalFixtureConstants({
        prefix: "maximum_observer",
        fold: observerScript.observerFieldTerminalFoldVector,
        preimageHex: observerScript.observerFieldPreimageCborHex,
        itemCount: terminalItemCount(
          observerScript.observerFieldPreimageCborHex,
          observerScript.observerFieldTerminalFoldVector,
        ),
      }),
      ...terminalFixtureConstants({
        prefix: "maximum_output",
        fold: outputs,
        preimageHex: outputs.fieldPreimageCborHex,
        itemCount: terminalItemCount(outputs.fieldPreimageCborHex, outputs),
      }),
      ...terminalFixtureConstants({
        prefix: "maximum_required_signer",
        fold: signerWitness.signerFieldTerminalFoldVector,
        preimageHex: signerWitness.signerFieldPreimageCborHex,
        itemCount: terminalItemCount(
          signerWitness.signerFieldPreimageCborHex,
          signerWitness.signerFieldTerminalFoldVector,
        ),
      }),
      ...terminalFixtureConstants({
        prefix: "maximum_mint",
        fold: mint,
        preimageHex: mint.fieldPreimageCborHex,
        itemCount: terminalItemCount(mint.fieldPreimageCborHex, mint),
      }),
    },
  },
  // The field-8 maximum is also the adversarial proof-fit case for the
  // `da-hash-preimage` framing rule, whose step 01 carries its own copy of the
  // signed leaf. Copies documented as "pinned in
  // `native-tx.max-redeemers.test.ak`" are exactly the drift this channel
  // exists to close: a stale copy stays green because the id it pins is the id
  // of the compact it pins beside it, while the shape is one the codec can no
  // longer emit. Binding every part of the leaf from the same vector is what
  // makes the comment true.
  {
    aiken: "onchain/aiken/validators/fraud-proofs/da-hash-preimage/step-01.ak",
    constants: {
      maximum_cardano_compact_cbor: bytes(spendRedeemer.compactCborHex),
      maximum_cardano_transaction_id: bytes(spendRedeemer.transactionIdHex),
      maximum_cardano_witness_set_compact_cbor: bytes(
        spendRedeemer.witnessSetCompactCborHex,
      ),
      maximum_cardano_field_preimage_lengths_cbor: bytes(
        spendRedeemer.fieldPreimageLengthsCborHex,
      ),
    },
  },
];

for (const family of AIKEN_FAMILIES) {
  const aikenPath = join(repositoryRoot, family.aiken);
  writeOrCheck(
    aikenPath,
    rebindAikenConstants({
      source: readFileSync(aikenPath, "utf8"),
      constants: family.constants,
    }),
  );
}
