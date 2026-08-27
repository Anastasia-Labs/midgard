/** Strict evidence preparation for the standalone withdrawal-mistag family. */
import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import { computeHash32 } from "@al-ft/midgard-core";
import {
  decodeMidgardAddressBytes,
  decodeMidgardTxOutput,
  encodeMidgardSpendInputItemV1,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterialV1 } from "@al-ft/midgard-validation";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

const domain = Buffer.from("MidgardWithdrawalV1", "utf8");

const sameOutRef = (
  left: SDK.OutputReference,
  right: SDK.OutputReference,
): boolean =>
  left.transactionId === right.transactionId &&
  left.outputIndex === right.outputIndex;

const proofSteps = (proof: SDK.Proof) =>
  proof.map((step) => {
    if ("Branch" in step) {
      return {
        type: "branch" as const,
        skip: Number(step.Branch.skip),
        neighbors: step.Branch.neighbors,
      };
    }
    if ("Fork" in step) {
      return {
        type: "fork" as const,
        skip: Number(step.Fork.skip),
        neighbor: {
          nibble: Number(step.Fork.neighbor.nibble),
          prefix: step.Fork.neighbor.prefix,
          root: step.Fork.neighbor.root,
        },
      };
    }
    return {
      type: "leaf" as const,
      skip: Number(step.Leaf.skip),
      neighbor: { key: step.Leaf.key, value: step.Leaf.value },
    };
  });

const verifyMpf = ({
  root,
  key,
  value,
  proof,
  membership,
  label,
}: {
  readonly root: string;
  readonly key: Buffer;
  readonly value?: Buffer;
  readonly proof: SDK.Proof;
  readonly membership: boolean;
  readonly label: string;
}): void => {
  const actual = MpfProof.fromJSON(key, value, proofSteps(proof)).verify(
    membership,
  );
  const actualHex =
    actual === null
      ? SDK.EMPTY_MERKLE_TREE_ROOT
      : Buffer.from(actual).toString("hex");
  if (actualHex !== root) {
    throw new Error(`${label} does not open its authenticated MPF root`);
  }
};

const verifyCountedMembership = async <K, V>({
  witness,
  expectedDomain,
  keyCbor,
  valueCbor,
  label,
}: {
  readonly witness: SDK.RootMembershipProof<K, V>;
  readonly expectedDomain: SDK.RootDomain;
  readonly keyCbor: string;
  readonly valueCbor: string;
  readonly label: string;
}): Promise<void> => {
  if (JSON.stringify(witness.domain) !== JSON.stringify(expectedDomain)) {
    throw new Error(`${label} carries the wrong counted-root domain`);
  }
  if (witness.count <= 0n) throw new Error(`${label} count must be positive`);
  const counted = await Effect.runPromise(
    SDK.commitCountedRootProgram({
      domain: witness.domain,
      phasRoot: witness.phas_root,
      count: witness.count,
    }),
  );
  if (counted !== witness.root) {
    throw new Error(`${label} counted root does not bind its PHAS root/count`);
  }
  verifyMpf({
    root: witness.phas_root,
    key: Buffer.from(keyCbor, "hex"),
    value: Buffer.from(valueCbor, "hex"),
    proof: witness.proof,
    membership: true,
    label,
  });
};

const valuesEqual = (
  output: ReturnType<typeof decodeMidgardTxOutput>["value"],
  expected: SDK.WithdrawalBody["l2_value"],
): boolean => {
  if (output.lovelace !== (expected.get("")?.get("") ?? 0n)) return false;
  const expectedPolicies = [...expected.entries()].filter(
    ([policy]) => policy !== "",
  );
  if (expectedPolicies.length !== output.assets.size) return false;
  return expectedPolicies.every(([policy, assets]) => {
    const actual = output.assets.get(policy);
    return (
      actual !== undefined &&
      actual.size === assets.size &&
      [...assets.entries()].every(
        ([assetName, quantity]) => actual.get(assetName) === quantity,
      )
    );
  });
};

const signatureIsValid = (info: SDK.WithdrawalInfo): boolean => {
  try {
    const [publicKeyHex, signatureHex] = info.signature;
    if (publicKeyHex.length !== 64 || signatureHex.length !== 128) return false;
    const publicKey = CML.PublicKey.from_bytes(
      Buffer.from(publicKeyHex, "hex"),
    );
    if (publicKey.hash().to_hex() !== info.body.l2_owner) return false;
    const message = computeHash32(
      Buffer.concat([
        domain,
        Buffer.from(SDK.withdrawalBodyBytesV1(info.body), "hex"),
      ]),
    );
    return publicKey.verify(
      message,
      CML.Ed25519Signature.from_raw_bytes(Buffer.from(signatureHex, "hex")),
    );
  } catch {
    return false;
  }
};

const infoHash = (info: SDK.WithdrawalInfo): string =>
  computeHash32(Buffer.from(SDK.withdrawalInfoBytesV1(info), "hex")).toString(
    "hex",
  );
const bodyHash = (body: SDK.WithdrawalBody): string =>
  computeHash32(Buffer.from(SDK.withdrawalBodyBytesV1(body), "hex")).toString(
    "hex",
  );

export type PrepareWithdrawalMistagV1Args = {
  readonly challengedHeaderHash: string;
  readonly committedWithdrawal: SDK.WithdrawalSourceMembershipProof;
  readonly eventToStep: SDK.EventToStepMembershipProof;
  readonly transitionStep: SDK.IndexedTraceProof;
  readonly ledgerEvidence: SDK.WithdrawalMistagLedgerEvidenceV1;
};

/**
 * Authenticates every retained opening, recomputes the exact predicate, and
 * refuses honestly tagged evidence before any L1 transaction is submitted.
 */
export const prepareWithdrawalMistagV1 = async ({
  challengedHeaderHash,
  committedWithdrawal,
  eventToStep,
  transitionStep,
  ledgerEvidence,
}: PrepareWithdrawalMistagV1Args): Promise<SDK.WithdrawalMistagPreparedEvidenceV1> => {
  if (!/^[0-9a-f]{56}$/u.test(challengedHeaderHash)) {
    throw new Error(
      "withdrawal-mistag challenged header hash must be 28-byte hex",
    );
  }
  await verifyCountedMembership({
    witness: committedWithdrawal,
    expectedDomain: "WithdrawalsRootDomain",
    keyCbor: SDK.committedWithdrawalKeyBytesV1(committedWithdrawal.key),
    valueCbor: SDK.committedWithdrawalValueBytesV1(committedWithdrawal.value),
    label: "withdrawal source",
  });
  await verifyCountedMembership({
    witness: eventToStep,
    expectedDomain: "EventToStepRootDomain",
    keyCbor: Data.to(eventToStep.key, SDK.EventKey),
    valueCbor: Data.to(eventToStep.value, SDK.EventToStepValue),
    label: "event-to-step",
  });
  await verifyCountedMembership({
    witness: transitionStep,
    expectedDomain: "TransitionTraceRootDomain",
    keyCbor: Data.to(transitionStep.key),
    valueCbor: Data.to(transitionStep.value, SDK.TransitionStep),
    label: "transition trace",
  });

  if (!("WithdrawalEventKey" in eventToStep.key)) {
    throw new Error("event-to-step key is not a withdrawal event");
  }
  if (
    !sameOutRef(
      eventToStep.key.WithdrawalEventKey.withdrawal_id,
      committedWithdrawal.key,
    ) ||
    eventToStep.value.phase !== "Withdrawal" ||
    transitionStep.value.phase !== "Withdrawal" ||
    transitionStep.key !== eventToStep.value.step_index ||
    transitionStep.value.step_index !== eventToStep.value.step_index ||
    !("WithdrawalEventKey" in transitionStep.value.event_key) ||
    !sameOutRef(
      transitionStep.value.event_key.WithdrawalEventKey.withdrawal_id,
      committedWithdrawal.key,
    )
  ) {
    throw new Error("withdrawal-mistag transition coordinate is inconsistent");
  }

  const info = committedWithdrawal.value;
  let outputPresent = false;
  let coreValid = false;
  let cardanoValueSize = 0n;
  const outrefKey = encodeMidgardSpendInputItemV1({
    txId: Buffer.from(info.body.l2_outref.transactionId, "hex"),
    outputIndex: Number(info.body.l2_outref.outputIndex),
  });

  if ("PresentLedgerOutput" in ledgerEvidence) {
    const outputCbor = Buffer.from(
      ledgerEvidence.PresentLedgerOutput.output_cbor,
      "hex",
    );
    const material = buildCanonicalMidgardLedgerOutputMaterialV1({
      outputIndex: Number(info.body.l2_outref.outputIndex),
      outputCbor,
    });
    // The descriptor bytes are the trie value, never the full output bytes.
    verifyMpf({
      root: transitionStep.value.pre_utxos_root,
      key: outrefKey,
      value: material.descriptorCbor,
      proof: ledgerEvidence.PresentLedgerOutput.membership_proof,
      membership: true,
      label: "withdrawal ledger output",
    });
    const output = decodeMidgardTxOutput(outputCbor);
    const address = decodeMidgardAddressBytes(output.address);
    const assetCount = [...output.value.assets.values()].reduce(
      (total, assets) => total + assets.size,
      0,
    );
    outputPresent = true;
    cardanoValueSize = BigInt(material.descriptor.cardanoValueSize);
    coreValid =
      address.paymentCredential.kind === "PubKey" &&
      address.paymentCredential.hash.toString("hex") === info.body.l2_owner &&
      valuesEqual(output.value, info.body.l2_value) &&
      assetCount <= SDK.WITHDRAWAL_MISTAG_MAXIMUM_ASSET_COUNT_V1 &&
      signatureIsValid(info);
  } else {
    verifyMpf({
      root: transitionStep.value.pre_utxos_root,
      key: outrefKey,
      proof: ledgerEvidence.AbsentLedgerOutput.non_membership_proof,
      membership: false,
      label: "withdrawal ledger non-membership",
    });
  }

  const payable = SDK.withdrawalMistagPayableV1({
    body: info.body,
    cardanoValueSize,
  });
  const actualValid = outputPresent && coreValid && payable;
  const claimedValid = SDK.withdrawalClaimsValidV1(info);
  const exactOutputBytes = SDK.withdrawalMistagExactPayoutOutputBytesV1({
    body: info.body,
    cardanoValueSize,
  });
  return {
    version: 1,
    challengedHeaderHash,
    committedWithdrawal,
    eventToStep,
    transitionStep,
    ledgerEvidence,
    withdrawalInfoHash: infoHash(info),
    withdrawalBodyHash: bodyHash(info.body),
    cardanoValueSize,
    outputPresent,
    coreValid,
    payable,
    actualValid,
    exactOutputBytes,
    requiredLovelace: SDK.withdrawalMistagMinimumLovelaceV1({
      body: info.body,
      cardanoValueSize,
    }),
    direction: SDK.withdrawalMistagDirectionV1({ claimedValid, actualValid }),
  };
};
