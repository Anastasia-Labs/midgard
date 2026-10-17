import {
  decodeMidgardTxOutput,
  encodeMidgardForcedTxCanonical,
  encodeMidgardTxOutput,
  ledgerEd25519SignatureReady,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { RejectCodes, validatePhaseASingle } from "@al-ft/midgard-validation";
import {
  makeMinAdaFundedExactSizeOutputItem,
  makeNativeTx,
  outRefFromByte,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { CML } from "@lucid-evolution/lucid";
import { beforeAll, expect, it } from "vitest";

import { forcedVerdictForRejection } from "../src/workflow/forced-rejection-reason.js";

// The ledger's Ed25519 check is libsodium's, which initialises
// asynchronously; production entry points await it, and so does this file.
beforeAll(() => ledgerEd25519SignatureReady());

it("keeps the late oversized output ordinal ahead of an invalid signature", () => {
  const key = CML.PrivateKey.from_normal_bytes(
    Buffer.concat([Buffer.alloc(31), Buffer.from([1])]),
  );
  const address = Buffer.from(
    CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(key.to_public().hash()),
    )
      .to_address()
      .to_raw_bytes(),
  );
  const outputs = [100, 16_385].map((size) =>
    encodeMidgardTxOutput({
      ...decodeMidgardTxOutput(makeMinAdaFundedExactSizeOutputItem(size)),
      address,
    }),
  );
  expect(outputs.map((output) => output.length)).toEqual([100, 16_385]);
  const transaction = makeNativeTx({
    privateKey: key,
    version: 1n,
    invalidVkeyWitness: true,
    spendInputs: [outRefFromByte(0x11)],
    outputs,
  });
  const phaseA = validatePhaseASingle(
    {
      txId: transaction.txId,
      txCbor: encodeMidgardForcedTxCanonical(transaction.tx),
      sourceKind: "forced",
      programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([]),
      arrivalSeq: 0n,
      createdAt: new Date(0),
    },
    {
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      minFeeA: 0n,
      minFeeB: 0n,
      expectedNetworkId: 0n,
      concurrency: 1,
      strictnessProfile: "phase1_midgard",
    },
  );
  expect(phaseA).toMatchObject({
    code: RejectCodes.InvalidFieldType,
    consensusPhase: "canonicalDecode",
  });
  if (!("code" in phaseA))
    throw new Error("Oversized output did not reach the forced verdict writer");
  // The actual Phase A rejection must be writable under the frozen machine
  // code. OutputNonCanonical maps to E_INVALID_OUTPUT, so using that arm for
  // this E_INVALID_FIELD_TYPE rejection stopped forced classification.
  expect(forcedVerdictForRejection(phaseA)).toStrictEqual({
    ForcedTxInvalid: {
      reason: {
        FieldItemWidthIllegal: { field_index: 2n, item_index: 1n },
      },
    },
  });
});
