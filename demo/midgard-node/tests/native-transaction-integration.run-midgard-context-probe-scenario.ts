import { type MidgardCekProgramMaterialEntry } from "@al-ft/midgard-core/cek-proof";
import { MidgardRedeemerTag } from "@al-ft/midgard-validation/midgard-redeemers";
import { CML, Constr } from "@lucid-evolution/lucid";

import {
  attachComputedScriptIntegrityHash,
  type BaseLedgerOutRefs,
  buildNativeTx,
  runBothPhases,
} from "./native-transaction-integration.build-native-tx.js";
import {
  makeMidgardContextProbeRedeemer,
  makeMintPreimage,
  makePlutusDataBytes,
  makePlutusIntegerData,
  makeProtectedScriptValueOutput,
  makeRedeemersPreimageCbor,
  makeScriptOutput,
} from "./native-transaction-integration.make-mint-preimage.js";
import {
  ALWAYS_SUCCEEDS_MINT_SCRIPT_HEX,
  ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
  makeOutput,
  makeOutRef,
  makePubKeyOutput,
  makeRawUplcWitness,
  makeSingleAssetValue,
  MIDGARD_CONTEXT_PROBE_SCRIPT_HEX,
  MIDGARD_OBSERVE_GUARD_SCRIPT_HEX,
  MIDGARD_RECEIVE_GUARD_SCRIPT_HEX,
  midgardV1Hash,
  TEST_ADDRESS,
} from "./native-transaction-integration.script-witness-item-to-versioned.js";

/**
 * Builds the ledger map every single-spend scenario in this suite needs: the
 * spend out-ref bound to `spendOutput`, plus the reference out-ref bound to
 * `referenceOutput` (defaulting to a plain 2 ADA output at `TEST_ADDRESS`).
 */
export const makeBaseLedger = (
  outRefs: BaseLedgerOutRefs,
  spendOutput: Buffer,
  referenceOutput: Buffer = makeOutput(TEST_ADDRESS, 2_000_000n),
): Map<string, Buffer> =>
  new Map<string, Buffer>([
    [outRefs.inputOutRef.toString("hex"), spendOutput],
    [outRefs.referenceInputOutRef.toString("hex"), referenceOutput],
  ]);

const makeScriptSpendPreState = (
  opts: BaseLedgerOutRefs & {
    readonly scriptHash: CML.ScriptHash;
    readonly datum?: CML.PlutusData;
    readonly referenceOutput?: Buffer;
  },
): Map<string, Buffer> =>
  makeBaseLedger(
    opts,
    makeScriptOutput(
      opts.scriptHash,
      3_000_000n,
      opts.datum === undefined ? undefined : { datum: opts.datum },
    ),
    opts.referenceOutput,
  );

export const runPlutusV3SpendScenario = async (opts: {
  readonly spendScript: CML.Script;
  readonly datum?: CML.PlutusData;
  readonly txOptions?: Parameters<typeof buildNativeTx>[0];
  readonly attachScriptIntegrityHash?: boolean;
  readonly referenceOutput?: Buffer;
  readonly phaseBOptions?: Parameters<typeof runBothPhases>[3];
  readonly programMaterial?: readonly MidgardCekProgramMaterialEntry[];
}) => {
  const base = buildNativeTx(opts.txOptions);
  const tx =
    opts.attachScriptIntegrityHash === false
      ? base
      : attachComputedScriptIntegrityHash(base, [CML.Language.PlutusV3]);
  const preState = makeScriptSpendPreState({
    inputOutRef: tx.inputOutRef,
    referenceInputOutRef: tx.referenceInputOutRef,
    scriptHash: opts.spendScript.hash(),
    datum: opts.datum,
    referenceOutput: opts.referenceOutput,
  });

  return {
    ...tx,
    ...(await runBothPhases(
      tx.txId,
      tx.txCbor,
      preState,
      opts.phaseBOptions,
      opts.programMaterial,
    )),
  };
};

type MidgardContextProbeRedeemerOverrides = Partial<{
  readonly expectedFirstInput: Buffer;
  readonly expectedSecondInput: Buffer;
  readonly expectedFirstReference: Buffer;
  readonly expectedSecondReference: Buffer;
  readonly expectedFirstOutputScriptHash: string;
  readonly expectedSecondOutputScriptHash: string;
  readonly expectedSigner: string;
  readonly expectedObserver: string;
  readonly expectedPolicy: string;
  readonly expectedMintQuantity: bigint;
  readonly expectedReceiveScriptHash: string;
}>;

export const runMidgardContextProbeScenario = async (opts?: {
  readonly marker?: number;
  readonly redeemerOverrides?: MidgardContextProbeRedeemerOverrides;
}) => {
  const signerKey = CML.PrivateKey.generate_ed25519();
  const probeHash = midgardV1Hash(MIDGARD_CONTEXT_PROBE_SCRIPT_HEX);
  const mintHash = midgardV1Hash(ALWAYS_SUCCEEDS_MINT_SCRIPT_HEX);
  const observerHash = midgardV1Hash(MIDGARD_OBSERVE_GUARD_SCRIPT_HEX);
  const receiveHash = midgardV1Hash(MIDGARD_RECEIVE_GUARD_SCRIPT_HEX);
  const secondOutputHash = midgardV1Hash(ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX);
  const scriptSpendOutRef = makeOutRef(0x10, 0n);
  const pubkeySpendOutRef = makeOutRef(0x30, 0n);
  const firstReferenceOutRef = makeOutRef(0x41, 0n);
  const secondReferenceOutRef = makeOutRef(0x42, 0n);
  const assetName = Buffer.from("10", "hex");
  const mintPolicyId = Buffer.from(mintHash, "hex");
  const emptyRedeemer = new Constr(0, []);
  const receiveRedeemer = 99n;
  const observeRedeemer = 77n;
  const base = buildNativeTx({
    spendInputOutRefs: [pubkeySpendOutRef, scriptSpendOutRef],
    referenceInputOutRefs: [secondReferenceOutRef, firstReferenceOutRef],
    mintPreimageCbor: makeMintPreimage([
      { policyId: mintPolicyId, assetName: assetName, quantity: 1n },
    ]),
    scriptWitnessItems: [
      makeRawUplcWitness(MIDGARD_CONTEXT_PROBE_SCRIPT_HEX),
      makeRawUplcWitness(ALWAYS_SUCCEEDS_MINT_SCRIPT_HEX),
      makeRawUplcWitness(MIDGARD_OBSERVE_GUARD_SCRIPT_HEX),
      makeRawUplcWitness(MIDGARD_RECEIVE_GUARD_SCRIPT_HEX),
    ],
    redeemerTxWitsPreimageCbor: makeRedeemersPreimageCbor([
      {
        tag: MidgardRedeemerTag.Spend,
        index: 0n,
        data: makeMidgardContextProbeRedeemer({
          expectedSpendScriptHash: probeHash,
          expectedOwnRef: scriptSpendOutRef,
          expectedFirstInput:
            opts?.redeemerOverrides?.expectedFirstInput ?? scriptSpendOutRef,
          expectedSecondInput:
            opts?.redeemerOverrides?.expectedSecondInput ?? pubkeySpendOutRef,
          expectedFirstReference:
            opts?.redeemerOverrides?.expectedFirstReference ??
            firstReferenceOutRef,
          expectedSecondReference:
            opts?.redeemerOverrides?.expectedSecondReference ??
            secondReferenceOutRef,
          expectedFirstOutputScriptHash:
            opts?.redeemerOverrides?.expectedFirstOutputScriptHash ??
            receiveHash,
          expectedSecondOutputScriptHash:
            opts?.redeemerOverrides?.expectedSecondOutputScriptHash ??
            secondOutputHash,
          expectedSigner:
            opts?.redeemerOverrides?.expectedSigner ??
            signerKey.to_public().hash().to_hex(),
          expectedObserver:
            opts?.redeemerOverrides?.expectedObserver ?? observerHash,
          expectedPolicy: opts?.redeemerOverrides?.expectedPolicy ?? mintHash,
          expectedAssetName: assetName,
          expectedMintQuantity:
            opts?.redeemerOverrides?.expectedMintQuantity ?? 1n,
          expectedMintRedeemer: emptyRedeemer,
          expectedObserveRedeemer: observeRedeemer,
          expectedReceiveScriptHash:
            opts?.redeemerOverrides?.expectedReceiveScriptHash ?? receiveHash,
          expectedReceiveRedeemer: receiveRedeemer,
        }),
      },
      { tag: MidgardRedeemerTag.Mint, index: 0n },
      {
        tag: MidgardRedeemerTag.Reward,
        index: 0n,
        data: makePlutusDataBytes(observeRedeemer),
      },
      {
        tag: MidgardRedeemerTag.Receiving,
        index: 0n,
        data: makePlutusDataBytes(receiveRedeemer),
      },
    ]),
    requiredObserverItems: [Buffer.from(observerHash, "hex")],
    networkId: 0n,
    scriptLanguages: ["MidgardV1"],
    witnessMode: "valid",
    witnessSignerPrivateKey: signerKey,
    outputCbors: [
      makeProtectedScriptValueOutput(
        CML.ScriptHash.from_hex(receiveHash),
        makeSingleAssetValue(3_000_000n, mintPolicyId, assetName, 1n),
      ),
      makeScriptOutput(CML.ScriptHash.from_hex(secondOutputHash), 1_000_000n),
    ],
  });
  const preState = new Map<string, Buffer>([
    [
      scriptSpendOutRef.toString("hex"),
      makeScriptOutput(CML.ScriptHash.from_hex(probeHash), 3_000_000n, {
        datum: makePlutusIntegerData(7n),
      }),
    ],
    [
      pubkeySpendOutRef.toString("hex"),
      makePubKeyOutput(signerKey.to_public().hash(), 1_000_000n),
    ],
    [
      firstReferenceOutRef.toString("hex"),
      makeOutput(TEST_ADDRESS, 2_000_000n),
    ],
    [
      secondReferenceOutRef.toString("hex"),
      makeOutput(TEST_ADDRESS, 2_000_000n),
    ],
  ]);
  return {
    ...(await runBothPhases(base.txId, base.txCbor, preState)),
    probeHash,
  };
};
