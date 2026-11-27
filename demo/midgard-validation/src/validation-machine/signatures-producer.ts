/**
 * The Signatures producer: the address witnesses, each checked as Phase A
 * checks it and folded into the signer set, then every required signer
 * proved a member of that set.
 */
import {
  appendMidgardValidationMerkleLeaf,
  encodeCbor,
  hashMidgardSignerLeaf,
  verifyLedgerEd25519Signature,
} from "@al-ft/midgard-core";
import { Effect } from "effect";

import { MIDGARD_ADDRESS_WITNESSES_FIELD_INDEX } from "./canonical-field-item.js";
import { encodeValidationFrontierPeaks } from "./control-encoding.js";
import { proofBinding } from "./prepare-producers.shared.js";
import {
  preparedFieldPreimage,
  preparedSignerProof,
  type PreparedValidationTransaction,
} from "./prepared-validation-transaction.js";
import type { SignatureScanControl } from "./trace-builder-types.js";
import {
  phaseContinues,
  type PhaseOutcome,
  phaseStops,
  type TraceWalk,
} from "./trace-walk.js";
import type { ValidationMachineWorkWitness } from "./types.js";

export const produceSignatures = (
  prepared: Pick<
    PreparedValidationTransaction,
    | "input"
    | "proofSource"
    | "contextCbor"
    | "fieldPreimages"
    | "resolutionScheduleHash"
    | "addressWitnessesCollection"
    | "requiredSignersCollection"
    | "addressWitnessScanItems"
    | "emptyValidationFrontier"
    | "canonicalSignerHashes"
    | "signerFrontier"
    | "signerLeafHashes"
  >,
  { push }: Pick<TraceWalk, "push">,
): Effect.Effect<PhaseOutcome, Error> =>
  Effect.gen(function* () {
    const { addressWitnessesCollection, requiredSignersCollection } = prepared;
    let control: SignatureScanControl = {
      stage: 0,
      addressCount: addressWitnessesCollection.items.length === 0 ? 0 : -1,
      requiredCount: requiredSignersCollection.items.length === 0 ? 0 : -1,
      addressSeen: 0,
      requiredSeen: 0,
      previousSignerHash: Buffer.alloc(0),
      signerFrontier: prepared.emptyValidationFrontier,
      firstInvalidWitnessIndex: -1,
    };
    const pushWitness = (
      auxiliary: ValidationMachineWorkWitness["auxiliary"] = null,
    ): void => {
      push(
        "signatures",
        encodeCbor([
          ...proofBinding(prepared),
          prepared.resolutionScheduleHash,
          BigInt(control.stage),
          BigInt(control.addressCount),
          BigInt(control.requiredCount),
          BigInt(control.addressSeen),
          BigInt(control.requiredSeen),
          control.previousSignerHash,
          BigInt(control.signerFrontier.count),
          encodeValidationFrontierPeaks(control.signerFrontier),
          BigInt(control.firstInvalidWitnessIndex),
        ]),
        auxiliary,
      );
    };
    const invalidSignature = () =>
      phaseStops(
        {
          AddressWitnessSignatureInvalid: {
            witness_index: BigInt(control.firstInvalidWitnessIndex),
          },
        },
        "signature scan found an invalid signature",
      );
    if (control.addressCount === 0) {
      pushWitness();
      control = { ...control, stage: 1 };
    } else {
      for (const scan of prepared.addressWitnessScanItems) {
        // The cursor opens the item at `addressSeen`.
        if (scan.item.itemIndex !== control.addressSeen) {
          return yield* Effect.fail(
            new Error("address-witness scan left physical order"),
          );
        }
        pushWitness({
          kind: "transactionFieldItem",
          fieldIndex: MIDGARD_ADDRESS_WITNESSES_FIELD_INDEX,
          fieldPreimage: preparedFieldPreimage(
            prepared,
            MIDGARD_ADDRESS_WITNESSES_FIELD_INDEX,
          ),
        });
        const newSigner = !scan.decoded.signerHash.equals(
          control.previousSignerHash,
        );
        const signerFrontier = newSigner
          ? appendMidgardValidationMerkleLeaf(
              control.signerFrontier,
              hashMidgardSignerLeaf(scan.decoded.signerHash),
            )
          : control.signerFrontier;
        // The ledger's own check, as Phase A runs it. A failure of the
        // check itself is raised and stops the build.
        const signatureIsValid = verifyLedgerEd25519Signature(
          scan.decoded.verificationKey,
          prepared.input.transactionId,
          scan.decoded.signature,
        );
        const addressSeen = control.addressSeen + 1;
        const addressCount =
          control.addressCount === -1
            ? addressWitnessesCollection.items.length
            : control.addressCount;
        const last = addressSeen === addressCount;
        control = {
          ...control,
          ...(last ? { stage: 1 } : {}),
          addressCount,
          addressSeen,
          previousSignerHash: last ? Buffer.alloc(0) : scan.decoded.signerHash,
          signerFrontier,
          firstInvalidWitnessIndex:
            control.firstInvalidWitnessIndex === -1 && !signatureIsValid
              ? scan.item.itemIndex
              : control.firstInvalidWitnessIndex,
        };
      }
    }
    if (control.stage !== 1) {
      return yield* Effect.fail(
        new Error("address-witness scan did not reach required signers"),
      );
    }
    if (control.requiredCount === 0) {
      pushWitness();
      if (control.firstInvalidWitnessIndex >= 0) return invalidSignature();
      control = { ...control, stage: 2 };
    } else {
      for (const item of requiredSignersCollection.items) {
        const signerProof = preparedSignerProof(prepared, item.bytes);
        pushWitness({
          kind: "requiredSignerItem",
          // No field or item index on the wire: the field is 4 by
          // construction and the item index is `control.required_seen`.
          fieldIndex: 4,
          fieldPreimage: preparedFieldPreimage(prepared, 4),
          signerProof,
        });
        if (signerProof.kind !== "membership") {
          return phaseStops(
            {
              RequiredSignerUnsigned: {
                signer_index: BigInt(control.requiredSeen),
              },
            },
            "required signer is absent",
          );
        }
        const requiredSeen = control.requiredSeen + 1;
        const requiredCount =
          control.requiredCount === -1
            ? requiredSignersCollection.items.length
            : control.requiredCount;
        control = { ...control, requiredCount, requiredSeen };
        if (requiredSeen === requiredCount) {
          if (control.firstInvalidWitnessIndex >= 0) return invalidSignature();
          control = { ...control, stage: 2 };
        }
      }
    }
    if (control.stage !== 2) {
      return yield* Effect.fail(
        new Error("required-signer scan did not reach its handoff"),
      );
    }
    pushWitness();
    return phaseContinues;
  });
