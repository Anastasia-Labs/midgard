import {
  deriveMidgardForcedTxFaultEvidenceMaterial,
  encodeCbor,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardForcedTxCanonical,
  MIDGARD_CONSENSUS_PROFILE,
  validateMidgardConsensusForcedTxCbor,
} from "@al-ft/midgard-core";
import { RejectCodes, validatePhaseASingle } from "@al-ft/midgard-validation";
import {
  makeMinAdaFundedExactSizeOutputItem,
  makeNativeTx,
  outRefFromByte,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { expect, it } from "vitest";

it.each([16384, 16385])(
  "rejects the canonical first fault before a malformed later address witness at output size %s",
  async (size) => {
    const spent = outRefFromByte(0x63);
    const native = makeNativeTx({
      spendInputs: [spent],
      outputs: [makeMinAdaFundedExactSizeOutputItem(size)],
    });
    const malformedAddress = encodeCbor([
      Buffer.alloc(31, 0x11),
      Buffer.alloc(65, 0x22),
    ]);
    expect(malformedAddress.length).toBe(101);
    const canonicalTransactionCbor = encodeMidgardForcedTxCanonical({
      version: native.tx.version,
      body: native.tx.body,
      witnessSet: {
        ...native.tx.witnessSet,
        addrTxWitsPreimageCbor: encodeCbor([malformedAddress]),
      },
    });
    expect(
      validateMidgardConsensusForcedTxCbor(canonicalTransactionCbor),
    ).toBeNull();
    const material = deriveMidgardForcedTxFaultEvidenceMaterial(
      canonicalTransactionCbor,
    );
    expect(material.canonical.witnessSet.addrTxWitsPreimageCbor).toEqual(
      encodeCbor([malformedAddress]),
    );
    const programMaterialSidecarCbor = encodeMidgardCekProgramMaterialSidecar(
      [],
    );
    const phaseA = validatePhaseASingle(
      {
        txId: material.transactionId,
        txCbor: canonicalTransactionCbor,
        sourceKind: "forced",
        programMaterialSidecarCbor,
        arrivalSeq: 0n,
        createdAt: new Date(0),
      },
      {
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        concurrency: 1,
        strictnessProfile: "phase1_midgard",
      },
    );
    expect(phaseA).toMatchObject({
      code: RejectCodes.InvalidFieldType,
      consensusPhase: "canonicalDecode",
    });
  },
  120_000,
);
