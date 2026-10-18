import {
  MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS,
  type MidgardValidationDispute,
} from "@al-ft/midgard-core";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import {
  CaseThreadDatum,
  validationCaseThreadDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  type Assets,
  type BuildTxWithRedeemer,
  CML,
  Constr,
  credentialToAddress,
  Data,
  Emulator,
  type EmulatorAccount,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  mintingPolicyToId,
  paymentCredentialOf,
  type Script,
  scriptFromNative,
  type TxSignBuilder,
  type UTxO,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { applyBlueprintParamsExact } from "../../src/runtime.js";
import { type Blueprint } from "./emulator/blueprints.js";
import {
  EMULATOR_PROTOCOL_PARAMETERS,
  network,
  readBlueprint,
  realBlueprintPath,
} from "./submit-init-emulator-shared.js";
import { threadTokenName } from "./validation-case-names.js";

export const TRACE_TITLES = {
  game: "fraud_proofs/validation_trace/game_v1.main.spend",
  boundary: "fraud_proofs/validation_trace/boundary_v1.main.spend",
  award: "fraud_proofs/validation_trace/award_v1.main.spend",
  timeout: "fraud_proofs/validation_trace/timeout_v1.main.spend",
  bond: "fraud_proofs/validation_trace/dispute_bond_v1.main.spend",
  threadExpiry: "fraud_proofs/validation_trace/thread_expiry_v1.main.withdraw",
} as const;

export const cap = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.limits.max_dispute_move_fee_lovelace,
);
export const fraudProofOutputLovelace = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.limits.fraud_proof_output_lovelace,
);
export const sideClockMs =
  SELECTED_DEPLOYMENT_PROFILE.timing.dispute_side_clock_ms;
export const maxRangeMs =
  SELECTED_DEPLOYMENT_PROFILE.timing.dispute_max_validity_range_ms;

/** A thread holds enough to pay many capped fees. */
export const threadLovelace = 40_000_000n;

export type DeployedScript = {
  readonly script: Script;
  readonly hash: string;
  readonly address: string;
};

/** The game, boundary and timeout validators over one thread policy. */
export type GameScene = {
  readonly lucid: LucidEvolution;
  readonly emulator: Emulator;
  /** Submits and pays collateral; a third party to the thread. */
  readonly wallet: EmulatorAccount;
  /** The challenger's signing key, bech32: the threads' fraud prover. */
  readonly proverSigningKey: string;
  readonly proverKey: string;
  readonly proverAddress: string;
  readonly threadPolicyId: string;
  readonly game: DeployedScript;
  readonly boundary: DeployedScript;
  readonly timeout: DeployedScript;
  /** The one-step resolver hashes the boundary routes to, by resolver index. */
  readonly resolverHashes: readonly string[];
  /** Reference inputs carrying scripts, by script hash; others go inline. */
  readonly scriptReferences: ReadonlyMap<string, UTxO>;
};

/** A game scene whose thread policy is a native script the wallet signs. */
export type SeededGameScene = GameScene & {
  readonly threadPolicy: Script;
};

let blueprint: Blueprint | undefined;

export const deploy = (
  title: string,
  params: readonly Data[],
): DeployedScript => {
  blueprint ??= readBlueprint(realBlueprintPath);
  const script: Script = {
    type: "PlutusV3",
    script: applyBlueprintParamsExact({ blueprint, title, params }),
  };
  return {
    script,
    hash: validatorToScriptHash(script),
    address: validatorToAddress(network, script),
  };
};

export const scriptAddress = (hash: string) =>
  credentialToAddress(network, { type: "Script", hash });

export const keyAddress = (hash: string) =>
  credentialToAddress(network, { type: "Key", hash });

/** The resolver hashes a scene's boundary routes to; no resolver runs here. */
export const standInResolverHashes = (): readonly string[] =>
  Array.from({ length: 14 }, (_, index) =>
    (index + 0x30).toString(16).repeat(28),
  );

/**
 * The game, boundary and timeout validators as deployed over thread policy
 * `threadPolicyId`, with the thread expiry and award they name.
 */
export const deployThreadHolders = (
  threadPolicyId: string,
  threadExpiryHash: string,
  awardHash: string,
) => {
  const resolverHashes = standInResolverHashes();
  const timeout = deploy(TRACE_TITLES.timeout, [
    threadPolicyId,
    threadExpiryHash,
  ]);
  const boundary = deploy(TRACE_TITLES.boundary, [
    [...resolverHashes],
    awardHash,
    threadPolicyId,
    threadExpiryHash,
  ]);
  const game = deploy(TRACE_TITLES.game, [
    boundary.hash,
    timeout.hash,
    threadPolicyId,
    threadExpiryHash,
  ]);
  return { game, boundary, timeout, resolverHashes };
};

export const newEmulator = async (lovelace = 30_000_000_000n) => {
  const wallet = generateEmulatorAccount({ lovelace });
  const emulator = new Emulator([wallet], EMULATOR_PROTOCOL_PARAMETERS);
  const lucid = await Lucid(emulator, network);
  lucid.selectWallet.fromSeed(wallet.seedPhrase);
  return { wallet, emulator, lucid };
};

export const newKey = () => {
  const signingKey = CML.PrivateKey.generate_ed25519();
  const hash = signingKey.to_public().hash().to_hex();
  return {
    signingKey: signingKey.to_bech32(),
    hash,
    address: keyAddress(hash),
  };
};

/**
 * The thread holders applied exactly as deployed, over a thread policy the
 * wallet controls: threads are seeded at any move without a case behind
 * them, for the game's own moves. The award and the thread expiry are the
 * real scripts over that policy.
 */
export const setUpGameScene = async (): Promise<SeededGameScene> => {
  const { wallet, emulator, lucid } = await newEmulator();
  const prover = newKey();
  const threadPolicy = scriptFromNative({
    type: "sig",
    keyHash: paymentCredentialOf(wallet.address).hash,
  });
  const threadPolicyId = mintingPolicyToId(threadPolicy);
  const threadExpiry = deploy(TRACE_TITLES.threadExpiry, [
    threadPolicyId,
    BigInt(MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS),
  ]);
  const award = deploy(TRACE_TITLES.award, [
    threadPolicyId,
    "ab".repeat(28),
    threadExpiry.hash,
  ]);
  return {
    lucid,
    emulator,
    wallet,
    proverSigningKey: prover.signingKey,
    proverKey: prover.hash,
    proverAddress: prover.address,
    threadPolicyId,
    threadPolicy,
    ...deployThreadHolders(threadPolicyId, threadExpiry.hash, award.hash),
    scriptReferences: new Map(),
  };
};

/** Which case and defender a thread belongs to: its header and token. */
export type ThreadIdentity = {
  readonly caseId: string;
  readonly startedAt: number;
  readonly defenderVkh: string;
};

export type Thread = {
  readonly utxo: UTxO;
  readonly dispute: MidgardValidationDispute;
  readonly identity: ThreadIdentity;
};

export const threadUnit = (scene: GameScene, identity: ThreadIdentity) =>
  `${scene.threadPolicyId}${threadTokenName(identity.caseId, identity.defenderVkh)}`;

/** A thread's datum at the game, boundary or timeout validator. */
export const threadDatum = (
  scene: GameScene,
  identity: ThreadIdentity,
  dispute: MidgardValidationDispute,
): string =>
  Data.to(
    {
      fraud_prover: scene.proverKey,
      data: validationCaseThreadDataFromCore({
        dispute,
        caseId: identity.caseId,
        startedAt: BigInt(identity.startedAt),
        defenderVkh: identity.defenderVkh,
      }),
    },
    CaseThreadDatum,
  );

/** A defender identity on a case of its own, started at `startedAt`. */
export const seededIdentity = (
  startedAt: number,
  index = 0,
): ThreadIdentity => ({
  caseId: (0xc0 + index).toString(16).repeat(32),
  startedAt,
  defenderVkh: (0xd0 + index).toString(16).repeat(28),
});

/**
 * Mints one thread token per thread under the wallet's policy and locks each
 * at `address`, in order.
 */
export const seedThreads = async (
  scene: SeededGameScene,
  address: string,
  threads: readonly {
    readonly identity: ThreadIdentity;
    readonly dispute: MidgardValidationDispute;
  }[],
  lovelace = threadLovelace,
): Promise<readonly UTxO[]> => {
  const units = threads.map(({ identity }) => threadUnit(scene, identity));
  let tx = scene.lucid
    .newTx()
    .mintAssets(Object.fromEntries(units.map((unit) => [unit, 1n])))
    .attach.MintingPolicy(scene.threadPolicy);
  threads.forEach(({ identity, dispute }, index) => {
    tx = tx.pay.ToContract(
      address,
      { kind: "inline", value: threadDatum(scene, identity, dispute) },
      { lovelace, [units[index]!]: 1n },
    );
  });
  const signed = await (await tx.complete()).sign.withWallet().complete();
  const hash = await signed.submit();
  await scene.lucid.awaitTx(hash);
  const created = await scene.lucid.utxosByOutRef(
    threads.map((_, outputIndex) => ({ txHash: hash, outputIndex })),
  );
  return threads.map(
    (_, outputIndex) =>
      created.find((utxo) => utxo.outputIndex === outputIndex)!,
  );
};

/** How a move's transaction is funded. */
export type Funding =
  /** The thread is the only input and pays `fee`, by default the cap. */
  | { readonly kind: "thread"; readonly fee?: bigint }
  /**
   * A wallet input pays the fee, and the thread outputs carry `skim` less
   * lovelace than the thread inputs.
   */
  | { readonly kind: "wallet"; readonly skim: bigint };

export const threadPays: Funding = { kind: "thread" };
export const walletPays: Funding = { kind: "wallet", skim: 0n };

export type ThreadSpend = {
  readonly utxo: UTxO;
  readonly script: Script;
  readonly redeemer: BuildTxWithRedeemer;
};

export type MoveOutput = {
  readonly address: string;
  readonly datum: string;
  readonly assets: Assets;
};

export type Move = {
  readonly spends: readonly ThreadSpend[];
  /** Outputs before funding; the first output absorbs the fee or skim. */
  readonly outputs: readonly MoveOutput[];
  readonly funding: Funding;
  /** Key hashes the transaction declares as required signers. */
  readonly signers?: readonly string[];
  readonly validFrom: number;
  readonly validTo: number;
};

const lovelaceOf = (assets: Assets) => BigInt(assets.lovelace ?? 0n);

/** Builds and locally evaluates a move; refusals surface here. */
export const buildMove = async (
  scene: GameScene,
  move: Move,
): Promise<TxSignBuilder> => {
  const fee = move.funding.kind === "thread" ? (move.funding.fee ?? cap) : 0n;
  const deduction = move.funding.kind === "thread" ? fee : move.funding.skim;
  const outputs = move.outputs.map((output, index) =>
    index === 0
      ? {
          ...output,
          assets: {
            ...output.assets,
            lovelace: lovelaceOf(output.assets) - deduction,
          },
        }
      : output,
  );
  let tx = scene.lucid.newTx();
  if (move.funding.kind === "wallet") {
    const [walletInput] = await scene.lucid.wallet().getUtxos();
    tx = tx.collectFrom([walletInput!]);
  }
  const references = new Map<string, UTxO>();
  for (const spend of move.spends) {
    tx = tx.collectFrom([spend.utxo], spend.redeemer);
    const hash = validatorToScriptHash(spend.script);
    const reference = scene.scriptReferences.get(hash);
    if (reference !== undefined) references.set(hash, reference);
    else tx = tx.attach.SpendingValidator(spend.script);
  }
  if (references.size > 0) tx = tx.readFrom([...references.values()]);
  for (const output of outputs) {
    tx = tx.pay.ToContract(
      output.address,
      { kind: "inline", value: output.datum },
      output.assets,
    );
  }
  for (const signer of move.signers ?? []) tx = tx.addSignerKey(signer);
  tx = tx.validFrom(move.validFrom).validTo(move.validTo);
  if (move.funding.kind === "wallet")
    return tx.complete({ localUPLCEval: true });
  // A thread-paid fee is exactly `fee`: the outputs leave exactly that
  // unspent, so there is no change output, and the wallet only supplies
  // collateral.
  return tx
    .setMinFee(fee)
    .complete({ coinSelection: false, localUPLCEval: true });
};

/** Signs with the wallet and, when the move names the prover, the prover. */
export const submitMove = async (
  scene: GameScene,
  move: Move,
): Promise<string> => {
  let signing = (await buildMove(scene, move)).sign.withWallet();
  if (move.signers?.includes(scene.proverKey)) {
    signing = signing.sign.withPrivateKey(scene.proverSigningKey);
  }
  const hash = await (await signing.complete()).submit();
  await scene.lucid.awaitTx(hash);
  return hash;
};

/** The output a submitted transaction created at `outputIndex`. */
export const createdAt = async (
  scene: Pick<GameScene, "lucid">,
  txHash: string,
  outputIndex: number,
): Promise<UTxO> => {
  const [utxo] = await scene.lucid.utxosByOutRef([{ txHash, outputIndex }]);
  if (utxo === undefined) throw new Error(`no output ${txHash}#${outputIndex}`);
  return utxo;
};

/** A validity window of `width` ms from `from`, and the bounds Plutus sees. */
export const moveWindow = (from: number, width: number = maxRangeMs) => ({
  validFrom: from,
  validTo: from + width,
  // Plutus sees the exclusive upper slot bound as `validTo - 1`.
  currentTimeLower: from,
  currentTimeUpper: from + width - 1,
});

/** An out-ref as the `InlineDatum` tag on a payout. */
export const tagOf = (utxo: UTxO) =>
  Data.to(new Constr(0, [utxo.txHash, BigInt(utxo.outputIndex)]));
