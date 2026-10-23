/** Fixtures for the prover funding permit's walk over our staging of a
 * staged builtin step's operands and over its spend back: our staging
 * address and another key's, three runs, a staging output of ours, the
 * permit policy and reservation, and the walk under them. */
import {
  midgardCekStagingAddress,
  midgardCekStagingDeposit,
  midgardCekStagingRunDatumCbor,
  midgardCekStagingScript,
} from "@al-ft/midgard-sdk";
import {
  CML,
  generateEmulatorAccount,
  paymentCredentialOf,
  scriptFromNative,
  type UTxO,
} from "@lucid-evolution/lucid";

import { exactResolvedOutputCbor } from "../../src/workflow/funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import {
  type ValidationDisputeStagingShape,
  validationDisputeStagingWalk,
} from "../../src/workflow/funding-reservation-permit.validation-dispute-staging-authority.js";

const COINS_PER_UTXO_BYTE = 4_310n;
export const RESERVE = 3_000_000n;
export const ADA = 1_000_000n;

export const ours = generateEmulatorAccount({ lovelace: 0n });
export const theirs = generateEmulatorAccount({ lovelace: 0n });
export const KEY = paymentCredentialOf(ours.address).hash;
export const STAGING = midgardCekStagingAddress("Custom", KEY);
export const OTHER_STAGING = midgardCekStagingAddress(
  "Custom",
  paymentCredentialOf(theirs.address).hash,
);

const chunk = (length: number, salt: number) =>
  Buffer.from(Array.from({ length }, (_, index) => (index * 7 + salt) & 0xff));
export const RUN = midgardCekStagingRunDatumCbor([
  chunk(4_095, 1),
  chunk(4_095, 2),
]);
export const NEXT = midgardCekStagingRunDatumCbor([chunk(1_000, 3)]);
export const LAST = midgardCekStagingRunDatumCbor([chunk(4_095, 4)]);
export const depositOf = (datumCbor: string) =>
  midgardCekStagingDeposit({
    address: STAGING,
    datumCbor,
    coinsPerUtxoByte: COINS_PER_UTXO_BYTE,
  });

export const STAGED_OUT_REF = `${"ab".repeat(32)}#0`;
const FUNDING_OUT_REF = `${"cd".repeat(32)}#1`;
export const CASE_UNIT = `${"ef".repeat(28)}0123`;
export const THREAD_OUT_REF = `${"12".repeat(32)}#0`;
export const DEADLINE_MS = 1_900_000_000_000;

export const policy = {
  category: "validationTraceDispute",
  deploymentFingerprint: "fingerprint",
  fundingPaymentKeyHash: KEY,
  protocolParameters: { coinsPerUtxoByte: COINS_PER_UTXO_BYTE.toString() },
  validationDispute: {
    moves: { walletMoveReserveLovelace: RESERVE.toString() },
  },
} as never;

/** The permit state of a reservation holding `plain` lovelace of plain
 * coins for its action. */
const stateHolding = (plain: bigint) =>
  ({
    snapshot: {
      walletAddress: ours.address,
      activeInputs: [
        {
          outRef: FUNDING_OUT_REF,
          role: "funding",
          assets: [],
          lovelace: plain.toString(),
        },
      ],
    },
    currentFundingOutRefs: [FUNDING_OUT_REF],
  }) as never;

export const stagingShape = (
  shape: Partial<
    Extract<ValidationDisputeStagingShape, { kind: "stage_operands" }>
  > = {},
): ValidationDisputeStagingShape => ({
  kind: "stage_operands",
  datumCborHex: RUN,
  remainingDatumCborHexes: [NEXT, LAST],
  walletPaidHops: 0,
  caseUnit: CASE_UNIT,
  threadOutRef: THREAD_OUT_REF,
  deadlineMs: DEADLINE_MS,
  ...shape,
});

/** Our staging output holding `RUN`, as the ledger shows it. */
export const stagedOutput: UTxO = {
  txHash: STAGED_OUT_REF.split("#")[0]!,
  outputIndex: 0,
  address: STAGING,
  assets: { lovelace: depositOf(RUN) },
  datum: RUN,
  datumHash: undefined,
  scriptRef: undefined,
};

export const reclaimShape: ValidationDisputeStagingShape = {
  kind: "reclaim_staging",
  outRef: STAGED_OUT_REF,
  resolvedOutputCborHex: exactResolvedOutputCbor(stagedOutput),
};

/** The lineage a staging of ours left for `output`. */
export const stagingLineage = (
  output: UTxO = stagedOutput,
  sourceActionKind = "stage_operands",
) => ({
  sourceActionKind,
  sourceOutputIndex: output.outputIndex,
  outRef: `${output.txHash}#${output.outputIndex.toString()}`,
  resolvedOutputCborHex: exactResolvedOutputCbor(output),
});

export const walkOf = (
  shape: ValidationDisputeStagingShape,
  {
    plain = 1_000n * ADA,
    lineage = shape.kind === "reclaim_staging" ? stagingLineage() : null,
  }: { readonly plain?: bigint; readonly lineage?: unknown } = {},
) =>
  validationDisputeStagingWalk({
    shape,
    actionId: "validation-trace-stage:run:0",
    policy,
    state: stateHolding(plain),
    moves: { walletMoveReserveLovelace: RESERVE.toString() },
    lineage,
  });

/** An output at `address` holding `lovelace`, with what is given. */
export const outputAt = (
  address: string,
  lovelace: bigint,
  {
    datum,
    byHash = false,
    token = false,
    scriptRef = false,
  }: {
    readonly datum?: string;
    readonly byHash?: boolean;
    readonly token?: boolean;
    readonly scriptRef?: boolean;
  } = {},
) => {
  const assets = CML.MultiAsset.new();
  if (token)
    assets.set(
      CML.ScriptHash.from_hex("00".repeat(28)),
      CML.AssetName.from_raw_bytes(Buffer.from("01", "hex")),
      1n,
    );
  const data =
    datum === undefined ? undefined : CML.PlutusData.from_cbor_hex(datum);
  return CML.TransactionOutput.new(
    CML.Address.from_bech32(address),
    token ? CML.Value.new(lovelace, assets) : CML.Value.from_coin(lovelace),
    data === undefined
      ? undefined
      : byHash
        ? CML.DatumOption.new_hash(CML.hash_plutus_data(data))
        : CML.DatumOption.new_datum(data),
    scriptRef
      ? CML.Script.new_native(
          CML.NativeScript.from_cbor_hex(midgardCekStagingScript(KEY).script),
        )
      : undefined,
  );
};

/** A witness set carrying `natives` and, when asked, a Plutus script. */
export const witnessesOf = (
  natives: readonly string[],
  { plutus = false }: { readonly plutus?: boolean } = {},
) => {
  const witnesses = CML.TransactionWitnessSet.new();
  if (natives.length > 0) {
    const list = CML.NativeScriptList.new();
    for (const native of natives)
      list.add(CML.NativeScript.from_cbor_hex(native));
    witnesses.set_native_scripts(list);
  }
  if (plutus) {
    const list = CML.PlutusV3ScriptList.new();
    list.add(
      CML.PlutusV3Script.from_raw_bytes(
        Buffer.from("4e4d01000033222220051200120011", "hex"),
      ),
    );
    witnesses.set_plutus_v3_scripts(list);
  }
  return witnesses;
};

export const OUR_SCRIPT = midgardCekStagingScript(KEY).script;
export const THEIR_SCRIPT = scriptFromNative({
  type: "sig",
  keyHash: paymentCredentialOf(theirs.address).hash,
}).script;
