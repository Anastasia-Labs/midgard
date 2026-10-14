import { type MidgardValidationDispute } from "@al-ft/midgard-core";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import {
  requireInputIndex,
  requireMintRedeemerIndex,
  validationDisputeDataFromCore,
  ValidationDisputeDatum,
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
import { makeIsolatedAlwaysSucceedsAuthenticatedValidator } from "./emulator/validators.make-isolated-always-succeeds-authenticated-validator.js";
import {
  EMULATOR_PROTOCOL_PARAMETERS,
  network,
  readBlueprint,
  realBlueprintPath,
} from "./submit-init-emulator-shared.js";

export const TRACE_TITLES = {
  game: "fraud_proofs/validation_trace/game_v1.main.spend",
  boundary: "fraud_proofs/validation_trace/boundary_v1.main.spend",
  award: "fraud_proofs/validation_trace/award_v1.main.spend",
  timeout: "fraud_proofs/validation_trace/timeout_v1.main.spend",
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

/** A thread holds enough to pay many capped fees and still finalize. */
export const threadLovelace = 40_000_000n;

export const headerHash = "aa".repeat(28);

/** The CT asset name: category ‖ header, shared by every thread on a header. */
export const threadName = `00000006${headerHash}`;

export type DeployedScript = {
  readonly script: Script;
  readonly hash: string;
  readonly address: string;
};

export type GameScene = {
  readonly lucid: LucidEvolution;
  readonly emulator: Emulator;
  /** Submits and pays collateral; a third party to the dispute. */
  readonly wallet: EmulatorAccount;
  /** The fraud prover's signing key, bech32. */
  readonly proverSigningKey: string;
  readonly proverKey: string;
  readonly proverAddress: string;
  readonly threadPolicy: Script;
  readonly threadUnit: string;
  readonly fraudProofPolicy: Script;
  readonly fraudProofUnit: string;
  readonly fraudProofAddress: string;
  readonly game: DeployedScript;
  readonly boundary: DeployedScript;
  readonly award: DeployedScript;
  readonly timeout: DeployedScript;
  /** The one-step resolver hashes the boundary routes to, by resolver index. */
  readonly resolverHashes: readonly string[];
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

/** `Address { Script(hash), None }` as Data. */
const scriptAddressData = (hash: string) =>
  new Constr(0, [new Constr(1, [hash]), new Constr(1, [])]);

export const scriptAddress = (hash: string) =>
  credentialToAddress(network, { type: "Script", hash });

/**
 * The game, boundary, award and timeout validators, applied exactly as
 * deployed over a CT policy the wallet controls and an always-succeeding
 * fraud-proof policy (finalize reads only that policy's redeemer and token).
 */
export const setUpGameScene = async (): Promise<GameScene> => {
  const wallet = generateEmulatorAccount({ lovelace: 30_000_000_000n });
  const proverSigningKey = CML.PrivateKey.generate_ed25519();
  const emulator = new Emulator([wallet], EMULATOR_PROTOCOL_PARAMETERS);
  const lucid = await Lucid(emulator, network);
  lucid.selectWallet.fromSeed(wallet.seedPhrase);
  const threadPolicy = scriptFromNative({
    type: "sig",
    keyHash: paymentCredentialOf(wallet.address).hash,
  });
  const threadPolicyId = mintingPolicyToId(threadPolicy);
  const fraudProof = makeIsolatedAlwaysSucceedsAuthenticatedValidator();
  const finalizeParams = [
    threadPolicyId,
    fraudProof.policyId,
    scriptAddressData(fraudProof.spendingScriptHash),
  ];
  const award = deploy(TRACE_TITLES.award, finalizeParams);
  const timeout = deploy(TRACE_TITLES.timeout, finalizeParams);
  const resolverHashes = Array.from({ length: 14 }, (_, index) =>
    (index + 0x30).toString(16).repeat(28),
  );
  const boundary = deploy(TRACE_TITLES.boundary, [
    [...resolverHashes],
    award.hash,
    threadPolicyId,
  ]);
  const game = deploy(TRACE_TITLES.game, [
    boundary.hash,
    timeout.hash,
    threadPolicyId,
  ]);
  const proverKey = proverSigningKey.to_public().hash().to_hex();
  return {
    lucid,
    emulator,
    wallet,
    proverSigningKey: proverSigningKey.to_bech32(),
    proverKey,
    proverAddress: credentialToAddress(network, {
      type: "Key",
      hash: proverKey,
    }),
    threadPolicy,
    threadUnit: `${threadPolicyId}${threadName}`,
    fraudProofPolicy: fraudProof.mintingScript,
    fraudProofUnit: `${fraudProof.policyId}${threadName}`,
    fraudProofAddress: fraudProof.spendingScriptAddress,
    game,
    boundary,
    award,
    timeout,
    resolverHashes,
  };
};

/** A thread's `StepDatum` over the game state for `dispute`. */
export const gameDatum = (
  scene: GameScene,
  dispute: MidgardValidationDispute,
): string =>
  Data.to(
    {
      fraud_prover: scene.proverKey,
      data: {
        challenged_header_hash: headerHash,
        dispute: validationDisputeDataFromCore(dispute),
      },
    },
    ValidationDisputeDatum,
  );

/** Mints one CT token per datum and locks each at `address`, in datum order. */
export const seedThreads = async (
  scene: GameScene,
  address: string,
  datums: readonly string[],
  lovelace = threadLovelace,
): Promise<readonly UTxO[]> => {
  let tx = scene.lucid
    .newTx()
    .mintAssets({ [scene.threadUnit]: BigInt(datums.length) })
    .attach.MintingPolicy(scene.threadPolicy);
  for (const datum of datums) {
    tx = tx.pay.ToContract(
      address,
      { kind: "inline", value: datum },
      { lovelace, [scene.threadUnit]: 1n },
    );
  }
  const signed = await (await tx.complete()).sign.withWallet().complete();
  const hash = await signed.submit();
  await scene.lucid.awaitTx(hash);
  const created = await scene.lucid.utxosByOutRef(
    datums.map((_, outputIndex) => ({ txHash: hash, outputIndex })),
  );
  return datums.map(
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
  /** Finalize: burn the CT token and mint the fraud-proof token. */
  readonly finalize?: { readonly mintRedeemer: BuildTxWithRedeemer };
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
  for (const spend of move.spends) {
    tx = tx
      .collectFrom([spend.utxo], spend.redeemer)
      .attach.SpendingValidator(spend.script);
  }
  if (move.finalize !== undefined) {
    tx = tx
      .mintAssets({ [scene.threadUnit]: -BigInt(move.spends.length) })
      .attach.MintingPolicy(scene.threadPolicy)
      .mintAssets(
        { [scene.fraudProofUnit]: BigInt(move.spends.length) },
        move.finalize.mintRedeemer,
      )
      .attach.MintingPolicy(scene.fraudProofPolicy);
  }
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

/** The output a submitted move created at `outputIndex`. */
export const createdAt = async (
  scene: GameScene,
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

/** `own_out_ref` as the `InlineDatum` tag on a finalize remainder. */
export const tagOf = (utxo: UTxO) =>
  Data.to(new Constr(0, [utxo.txHash, BigInt(utxo.outputIndex)]));

/** The fraud-proof token's datum, `fraud_proof.Datum { fraud_prover }`. */
export const fraudProofDatum = (scene: GameScene) =>
  Data.to(new Constr(0, [scene.proverKey]));

/**
 * `fraud_proof.MintRedeemer`, naming the burnt CT asset; the CT policy here is
 * native, so its redeemer index is never read.
 */
export const fraudProofMintRedeemer: BuildTxWithRedeemer = () =>
  Data.to(new Constr(0, [threadName, 0n]));

/**
 * A finalize of `thread` through `script` with redeemer `redeem(args)`: the
 * fraud-proof output first, then the remainder to the prover.
 */
export const finalizeMove = (
  scene: GameScene,
  thread: UTxO,
  script: Script,
  redeem: (args: {
    readonly input_index: bigint;
    readonly output_index: bigint;
    readonly fraud_proof_mint_redeemer_index: bigint;
  }) => string,
  funding: Funding,
  validFrom: number,
): Move => {
  const fraudProofOutput: MoveOutput = {
    address: scene.fraudProofAddress,
    datum: fraudProofDatum(scene),
    assets: {
      lovelace: fraudProofOutputLovelace,
      [scene.fraudProofUnit]: 1n,
    },
  };
  const remainder: MoveOutput = {
    address: scene.proverAddress,
    datum: tagOf(thread),
    assets: { lovelace: lovelaceOf(thread.assets) - fraudProofOutputLovelace },
  };
  return {
    spends: [
      {
        utxo: thread,
        script,
        redeemer: (ctx) =>
          redeem({
            input_index: requireInputIndex(ctx, thread, "finalize"),
            output_index: 1n,
            fraud_proof_mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              mintingPolicyToId(scene.fraudProofPolicy),
              "finalize",
            ),
          }),
      },
    ],
    // The remainder absorbs the fee or skim; the fraud-proof output stays at
    // exactly the profile amount.
    outputs: [remainder, fraudProofOutput],
    funding,
    validFrom,
    validTo: validFrom + maxRangeMs,
    finalize: { mintRedeemer: fraudProofMintRedeemer },
  };
};
