import {
  type LucidEvolution,
  type Script,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import { dedupeByOutRef } from "@/tx-context.js";
import {
  fetchReferenceScriptUtxosProgram,
  type ReferenceScriptResolved,
} from "@/transactions/reference-scripts.js";

export type ReservePayoutReferenceScripts = {
  readonly depositMinting?: UTxO;
  readonly depositSpending?: UTxO;
  readonly depositWitnessCertificate?: UTxO;
  readonly withdrawalMinting?: UTxO;
  readonly withdrawalSpending?: UTxO;
  readonly withdrawalWitnessCertificate?: UTxO;
  readonly membershipProofWithdrawal?: UTxO;
  readonly reserveSpending?: UTxO;
  readonly payoutSpending?: UTxO;
  readonly payoutMinting?: UTxO;
};

export const resolveReferenceScriptsProgram = (
  lucid: LucidEvolution,
  address: string | undefined,
  targets: readonly {
    readonly name: string;
    readonly script: Script;
  }[],
  explicit?: ReservePayoutReferenceScripts,
): Effect.Effect<readonly ReferenceScriptResolved[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    if (address === undefined) {
      return [];
    }
    const unresolvedTargets = targets.filter(
      (target) => !hasExplicitReferenceScript(explicit, target.name),
    );
    if (unresolvedTargets.length <= 0) {
      return [];
    }
    return yield* fetchReferenceScriptUtxosProgram(
      lucid,
      address,
      unresolvedTargets,
    );
  });

const hasExplicitReferenceScript = (
  explicit: ReservePayoutReferenceScripts | undefined,
  name: string,
): boolean => {
  if (explicit === undefined) {
    return false;
  }
  switch (name) {
    case "deposit minting":
      return explicit.depositMinting !== undefined;
    case "deposit spending":
      return explicit.depositSpending !== undefined;
    case "deposit witness certificate":
      return explicit.depositWitnessCertificate !== undefined;
    case "withdrawal minting":
      return explicit.withdrawalMinting !== undefined;
    case "withdrawal spending":
      return explicit.withdrawalSpending !== undefined;
    case "withdrawal witness certificate":
      return explicit.withdrawalWitnessCertificate !== undefined;
    case "membership proof withdrawal":
      return explicit.membershipProofWithdrawal !== undefined;
    case "reserve spending":
      return explicit.reserveSpending !== undefined;
    case "payout spending":
      return explicit.payoutSpending !== undefined;
    case "payout minting":
      return explicit.payoutMinting !== undefined;
    default:
      return false;
  }
};

export const mergeReferenceScripts = (
  explicit: ReservePayoutReferenceScripts | undefined,
  resolved: readonly ReferenceScriptResolved[],
): ReservePayoutReferenceScripts => ({
  ...explicit,
  depositMinting:
    explicit?.depositMinting ??
    resolved.find((entry) => entry.name === "deposit minting")?.utxo,
  depositSpending:
    explicit?.depositSpending ??
    resolved.find((entry) => entry.name === "deposit spending")?.utxo,
  depositWitnessCertificate:
    explicit?.depositWitnessCertificate ??
    resolved.find((entry) => entry.name === "deposit witness certificate")
      ?.utxo,
  withdrawalMinting:
    explicit?.withdrawalMinting ??
    resolved.find((entry) => entry.name === "withdrawal minting")?.utxo,
  withdrawalSpending:
    explicit?.withdrawalSpending ??
    resolved.find((entry) => entry.name === "withdrawal spending")?.utxo,
  withdrawalWitnessCertificate:
    explicit?.withdrawalWitnessCertificate ??
    resolved.find((entry) => entry.name === "withdrawal witness certificate")
      ?.utxo,
  membershipProofWithdrawal:
    explicit?.membershipProofWithdrawal ??
    resolved.find((entry) => entry.name === "membership proof withdrawal")
      ?.utxo,
  reserveSpending:
    explicit?.reserveSpending ??
    resolved.find((entry) => entry.name === "reserve spending")?.utxo,
  payoutSpending:
    explicit?.payoutSpending ??
    resolved.find((entry) => entry.name === "payout spending")?.utxo,
  payoutMinting:
    explicit?.payoutMinting ??
    resolved.find((entry) => entry.name === "payout minting")?.utxo,
});

export const referenceInputs = (
  hubOracleRefInput: UTxO,
  additional: readonly (UTxO | undefined)[],
): readonly UTxO[] =>
  dedupeByOutRef([
    hubOracleRefInput,
    ...additional.filter((utxo): utxo is UTxO => utxo !== undefined),
  ]);

export const attachIfMissing = (
  tx: TxBuilder,
  script: Script,
  referenceScript: UTxO | undefined,
): TxBuilder => (referenceScript === undefined ? tx.attach.Script(script) : tx);
