import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeHash28,
  decodeMidgardNativeTxCompact,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxCompact } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import {
  invalidRangeViolationReason,
  nativeTxBodyHasZeroInputViolation,
  normalizeNativeTxValidityRange,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  type InvalidRangeEvidence,
  invalidRangeEvidenceCloses,
  prepareInvalidRangeEvidence,
} from "../invalid-range/family.js";
import {
  forcedTxFromCoreCompact,
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../step-support.js";
import {
  prepareZeroInputEvidence,
  type ZeroInputEvidence,
  ZeroInputVerdictSubjectSchema,
} from "../zero-input/family.js";
import { ZeroInputForcedSourcePayloadSchema } from "../zero-input/schemas.js";
import {
  type NativeInclusionTwoStepArtifact,
  parseArtifact,
  proofSteps,
} from "./native-inclusion-two-step.parse-artifact.js";

export const admitNativeInclusionTwoStepArtifact = (
  value: unknown,
): Readonly<{
  artifact: NativeInclusionTwoStepArtifact;
  inclusion: ReturnType<typeof parseSubmitStep01TxInclusion> | null;
  zeroInputEvidence: ZeroInputEvidence | null;
  invalidRangeEvidence: InvalidRangeEvidence | null;
  forcedSource: Readonly<Record<string, unknown>> | null;
}> => {
  const artifact = parseArtifact(value);
  const compact = (
    artifact.sourceKind === "forced"
      ? decodeMidgardForcedTxCompact
      : decodeMidgardNativeTxCompact
  )(Buffer.from(artifact.nativeTxCompactCbor, "hex"));
  const inclusion =
    artifact.sourceKind === "accepted"
      ? parseSubmitStep01TxInclusion({
          nativeTxId: artifact.nativeTxId,
          nativeTx: nativeTxFromCoreCompact(
            decodeMidgardNativeTxCompact(
              Buffer.from(artifact.nativeTxCompactCbor, "hex"),
            ),
          ),
          nativeTxCompactCbor: artifact.nativeTxCompactCbor,
          l2TransactionSourceCbor: artifact.l2TransactionSourceCbor,
          transactionsPhasRoot: artifact.transactionsPhasRoot,
          txMembershipProofCbor: artifact.txMembershipProofCbor,
        })
      : null;
  let openedRoot: Buffer | null;
  try {
    if (inclusion === null) throw new Error("forced source");
    openedRoot = MpfProof.fromJSON(
      Buffer.from(artifact.nativeTxId, "hex"),
      Buffer.from(artifact.l2TransactionSourceCbor, "hex"),
      proofSteps(inclusion.txMembershipProof),
    ).verify(true);
  } catch {
    if (artifact.sourceKind === "forced") openedRoot = null;
    else
      throw new Error(
        "native-inclusion artifact membership proof cannot be replayed",
      );
  }
  if (
    artifact.sourceKind === "accepted" &&
    (openedRoot === null ||
      openedRoot.toString("hex") !== artifact.transactionsPhasRoot)
  ) {
    throw new Error(
      "native-inclusion artifact membership proof does not open its PHAS root",
    );
  }
  let invalidRangeEvidence: InvalidRangeEvidence | null = null;
  if (
    artifact.category === "invalidRange" &&
    artifact.sourceKind === "accepted"
  ) {
    const reason = invalidRangeViolationReason({
      blockSlot: BigInt(artifact.blockSlot!),
      normalizedRange: normalizeNativeTxValidityRange(inclusion!.nativeTx.body),
    });
    const expectedDetection = `invalid-range:${artifact.position.toString()}:${artifact.nativeTxId}:${reason ?? "none"}`;
    if (
      reason === null ||
      reason !== artifact.violationReason ||
      artifact.detectionId !== expectedDetection
    ) {
      throw new Error(
        "invalid-range artifact does not re-derive its selected violation",
      );
    }
    const subject = Data.from(
      artifact.subjectCbor,
      SDK.InvalidRangeVerdictSubjectSchema as never,
    ) as SDK.VerdictSubject;
    invalidRangeEvidence = prepareInvalidRangeEvidence({
      subject,
      blockSlot: BigInt(artifact.blockSlot!),
      txBody: inclusion!.nativeTx.body,
    });
    if (
      subject.direction !== 0n ||
      !invalidRangeEvidenceCloses(invalidRangeEvidence) ||
      artifact.forcedSourceCbor !== ""
    )
      throw new Error("invalid-range accepted artifact source changed");
  } else if (artifact.category === "zeroInput") {
    if (
      artifact.sourceKind === "accepted" &&
      (!nativeTxBodyHasZeroInputViolation({
        txBody: inclusion!.nativeTx.body,
      }) ||
        artifact.detectionId !==
          `zero-input:${artifact.position.toString()}:${artifact.nativeTxId}`)
    ) {
      throw new Error(
        "zero-input artifact does not re-derive its selected violation",
      );
    }
  }
  let zeroInputEvidence: ZeroInputEvidence | null = null;
  let forcedSource: Readonly<Record<string, unknown>> | null = null;
  if (artifact.category === "zeroInput") {
    const subject = Data.from(
      artifact.subjectCbor,
      ZeroInputVerdictSubjectSchema as never,
    ) as SDK.VerdictSubject;
    zeroInputEvidence = prepareZeroInputEvidence({
      finding: { subject },
      inputFieldPreimage: Buffer.from(artifact.inputFieldPreimageCbor, "hex"),
      committedFieldHashHex: artifact.inputFieldCommitment,
    });
    if (
      compact.transactionBody.spendInputsHash.toString("hex") !==
        artifact.inputFieldCommitment ||
      zeroInputEvidence.subject.transaction_id !== artifact.nativeTxId
    ) {
      throw new Error("zero-input artifact field evidence changed transaction");
    }
    if (artifact.sourceKind === "accepted") {
      if (artifact.forcedSourceCbor !== "" || subject.direction !== 0n)
        throw new Error("zero-input accepted artifact source changed");
    } else {
      if (
        artifact.txMembershipProofCbor !== "" ||
        artifact.transactionsPhasRoot !== "00".repeat(32)
      )
        throw new Error("zero-input forced artifact carried accepted evidence");
      forcedSource = Data.from(
        artifact.forcedSourceCbor,
        ZeroInputForcedSourcePayloadSchema as never,
      ) as Readonly<Record<string, unknown>>;
      const source = forcedSource as {
        readonly header: SDK.Header;
        readonly membership: SDK.ForcedTransactionSourceMembershipProof;
        readonly direction: bigint;
      };
      const leaf = source.membership.value;
      if (
        computeHash28(SDK.encodeHeaderCbor(source.header)).toString("hex") !==
          artifact.headerHash ||
        source.direction !== 1n ||
        source.membership.root !== source.header.forcedTransactionsRoot ||
        source.membership.count !== source.header.forcedTransactionCount ||
        leaf.tx_id !== artifact.nativeTxId ||
        leaf.submitted_source.compact_cbor !== artifact.nativeTxCompactCbor ||
        Data.to(
          { tx_id: leaf.tx_id, source: leaf.submitted_source } as never,
          SDK.L2TransactionSource as never,
        ) !== artifact.l2TransactionSourceCbor ||
        leaf.verdict === "ForcedTxValid" ||
        leaf.verdict.ForcedTxInvalid.reason !== "EmptyInputs"
      )
        throw new Error(
          "zero-input forced artifact changed authenticated leaf",
        );
      const derivedSubject = SDK.forcedVerdictSubject({
        transactionId: leaf.tx_id,
        sourceKey: source.membership.key,
        rejectionReason: leaf.verdict.ForcedTxInvalid.reason,
      });
      if (
        Data.to(
          derivedSubject as never,
          ZeroInputVerdictSubjectSchema as never,
        ) !== artifact.subjectCbor
      )
        throw new Error(
          "zero-input forced artifact injected its verdict subject",
        );
      let forcedRoot: Buffer | null;
      try {
        forcedRoot = MpfProof.fromJSON(
          Buffer.from(
            Data.to(
              source.membership.key as never,
              SDK.OutputReferenceSchema as never,
            ),
            "hex",
          ),
          Buffer.from(
            Data.to(leaf as never, SDK.ForcedInclusionTxV1Schema as never),
            "hex",
          ),
          proofSteps(source.membership.proof as never),
        ).verify(true);
      } catch {
        throw new Error(
          "zero-input forced artifact membership cannot be replayed",
        );
      }
      if (forcedRoot?.toString("hex") !== source.membership.phas_root)
        throw new Error("zero-input forced artifact membership root changed");
    }
  } else if (artifact.sourceKind === "forced") {
    const subject = Data.from(
      artifact.subjectCbor,
      SDK.InvalidRangeVerdictSubjectSchema as never,
    ) as SDK.VerdictSubject;
    const source = Data.from(
      artifact.forcedSourceCbor,
      SDK.InvalidRangeForcedSourcePayloadSchema as never,
    ) as {
      header: SDK.Header;
      membership: SDK.ForcedTransactionSourceMembershipProof;
      direction: bigint;
    };
    const leaf = source.membership.value;
    if (
      computeHash28(SDK.encodeHeaderCbor(source.header)).toString("hex") !==
        artifact.headerHash ||
      source.direction !== 1n ||
      source.membership.root !== source.header.forcedTransactionsRoot ||
      source.membership.count !== source.header.forcedTransactionCount ||
      leaf.tx_id !== artifact.nativeTxId ||
      leaf.submitted_source.compact_cbor !== artifact.nativeTxCompactCbor ||
      Data.to(
        { tx_id: leaf.tx_id, source: leaf.submitted_source } as never,
        SDK.L2TransactionSource as never,
      ) !== artifact.l2TransactionSourceCbor ||
      artifact.txMembershipProofCbor !== "" ||
      artifact.transactionsPhasRoot !== "00".repeat(32) ||
      leaf.verdict === "ForcedTxValid" ||
      (leaf.verdict.ForcedTxInvalid.reason !== "ValidityIntervalMalformed" &&
        leaf.verdict.ForcedTxInvalid.reason !==
          "ValidityIntervalExcludesBlockSlot")
    )
      throw new Error(
        "invalid-range forced artifact changed authenticated leaf",
      );
    const derived = SDK.forcedVerdictSubject({
      transactionId: leaf.tx_id,
      sourceKey: source.membership.key,
      rejectionReason: leaf.verdict.ForcedTxInvalid.reason,
    });
    if (
      Data.to(
        derived as never,
        SDK.InvalidRangeVerdictSubjectSchema as never,
      ) !== artifact.subjectCbor
    )
      throw new Error(
        "invalid-range forced artifact injected its verdict subject",
      );
    let root: Buffer | null;
    try {
      root = MpfProof.fromJSON(
        Buffer.from(
          Data.to(
            source.membership.key as never,
            SDK.OutputReferenceSchema as never,
          ),
          "hex",
        ),
        Buffer.from(
          Data.to(leaf as never, SDK.ForcedInclusionTxV1Schema as never),
          "hex",
        ),
        proofSteps(source.membership.proof as never),
      ).verify(true);
    } catch {
      throw new Error(
        "invalid-range forced artifact membership cannot be replayed",
      );
    }
    if (root?.toString("hex") !== source.membership.phas_root)
      throw new Error("invalid-range forced artifact membership root changed");
    invalidRangeEvidence = prepareInvalidRangeEvidence({
      subject,
      blockSlot: source.header.blockSlot,
      txBody: forcedTxFromCoreCompact(compact).body,
    });
    if (
      !invalidRangeEvidenceCloses(invalidRangeEvidence) ||
      artifact.blockSlot !== source.header.blockSlot.toString() ||
      artifact.violationReason !== leaf.verdict.ForcedTxInvalid.reason ||
      artifact.detectionId !==
        `invalid-range:forced:${artifact.position.toString()}:${artifact.nativeTxId}:${leaf.verdict.ForcedTxInvalid.reason}`
    )
      throw new Error(
        "invalid-range forced artifact does not contradict rejection",
      );
    forcedSource = source as unknown as Readonly<Record<string, unknown>>;
  } else if (
    artifact.inputFieldPreimageCbor !== "" ||
    artifact.inputFieldCommitment !== "00".repeat(32) ||
    artifact.forcedSourceCbor !== ""
  ) {
    throw new Error("invalid-range artifact carried zero-input authority");
  }
  return Object.freeze({
    artifact,
    inclusion,
    invalidRangeEvidence,
    zeroInputEvidence,
    forcedSource,
  });
};
