/**
 * Shared harness for the user submission suites: an in-memory attempt store,
 * crash injection, funded users and readers of what the chain shows.
 */
import {
  addressDataFromBech32,
  eventHistoryKey,
  signWithdrawalBodyCbor,
  type SubmitDepositConfig,
  submitDepositProgram,
  type SubmitWithdrawalConfig,
  submitWithdrawalProgram,
  type UserL1View,
  type UserSubmissionAttempt,
  UserSubmissionError,
  type UserSubmissionOptions,
  type UserSubmissionOutcome,
  type UserSubmissionPending,
  type UserSubmissionStore,
  WithdrawalBody,
} from "@al-ft/midgard-sdk";
import { type UserHistoryScene } from "@al-ft/midgard-sdk/test-support/user-history";
import {
  CML,
  Data,
  type EmulatorAccount,
  generateEmulatorAccount,
  getAddressDetails,
  type LucidEvolution,
  type OutRef,
  type UTxO,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Effect, type Either } from "effect";

// Each case runs genuine list scripts through local UPLC evaluation, and the
// scene deploys and initializes both lists before the first case.
export const SCENE_TIMEOUT_MS = 300_000;
export const CASE_TIMEOUT_MS = 300_000;
export const HOUR_MS = 3_600_000;

export type Kind = "Deposit" | "Withdrawal";

export type User = {
  readonly account: EmulatorAccount;
  readonly lucid: LucidEvolution;
  readonly view: UserL1View;
};

export type RunResult = {
  readonly result: Either.Either<
    UserSubmissionOutcome,
    UserSubmissionError | UserSubmissionPending
  >;
  readonly lines: readonly string[];
};

export const memoryStore = () => {
  const attempts: UserSubmissionAttempt[] = [];
  const store: UserSubmissionStore = {
    latest: async () => attempts.at(-1),
    append: async (attempt) => {
      if (attempts.some((recorded) => recorded.attempt === attempt.attempt))
        return "lost";
      attempts.push(attempt);
      return "created";
    },
  };
  return { attempts, store };
};

/** The process dies once, at the first append `when` selects: after the
 * attempt is created, or before anything is written. */
export const crashOnce = (
  store: UserSubmissionStore,
  at: "after-create" | "before-create",
  when: (attempt: UserSubmissionAttempt) => boolean = () => true,
): UserSubmissionStore => {
  let armed = true;
  return {
    latest: store.latest,
    append: async (attempt) => {
      if (!armed || !when(attempt)) return store.append(attempt);
      armed = false;
      if (at === "after-create") await store.append(attempt);
      throw new Error("simulated crash");
    },
  };
};

export const newUser = async (
  scene: UserHistoryScene,
  coins: readonly bigint[] = [40_000_000n, 20_000_000n],
): Promise<User> => {
  const account = generateEmulatorAccount({});
  let tx = scene.operator.newTx();
  for (const lovelace of coins)
    tx = tx.pay.ToAddress(account.address, { lovelace });
  await (await (await tx.complete()).sign.withWallet().complete()).submit();
  scene.emulator.awaitBlock();
  const lucid = await scene.lucidFor(account.seedPhrase);
  return {
    account,
    lucid,
    view: { lucid, indexedSlot: async () => scene.emulator.slot },
  };
};

export const addressData = (address: string) =>
  Effect.runSync(addressDataFromBech32(address));

export const depositConfig = (
  scene: UserHistoryScene,
  user: User,
  lovelace: bigint,
): SubmitDepositConfig => ({
  l2Address: user.account.address,
  l2Datum: null,
  lovelace,
  additionalAssets: {},
  referenceScripts: { depositMinting: scene.referenceScripts.depositMinting },
});

export const withdrawalConfig = (
  scene: UserHistoryScene,
  account: EmulatorAccount,
  lovelace: bigint,
): SubmitWithdrawalConfig => {
  const body: WithdrawalBody = {
    l2_outref: {
      transactionId: "ab".repeat(32),
      outputIndex: lovelace / 1_000_000n,
    },
    l2_owner: getAddressDetails(account.address).paymentCredential!.hash,
    l2_value: new Map([["", new Map([["", lovelace]])]]),
    l1_address: addressData(account.address),
    l1_datum: "NoDatum",
  };
  return {
    body,
    signature: signWithdrawalBodyCbor(
      CML.PrivateKey.from_bech32(
        walletFromSeed(account.seedPhrase, { network: "Custom" }).paymentKey,
      ),
      Data.to(body, WithdrawalBody),
    ),
    refundAddress: addressData(account.address),
    referenceScripts: {
      withdrawalMinting: scene.referenceScripts.withdrawalMinting,
    },
  };
};

export const submit = async (
  scene: UserHistoryScene,
  kind: Kind,
  user: Pick<User, "account" | "view">,
  store: UserSubmissionStore,
  {
    amount = 10_000_000n,
    config = {},
    options = {},
  }: {
    readonly amount?: bigint;
    readonly config?: Partial<SubmitDepositConfig & SubmitWithdrawalConfig>;
    readonly options?: Partial<UserSubmissionOptions>;
  } = {},
): Promise<RunResult> => {
  const lines: string[] = [];
  const runOptions: UserSubmissionOptions = {
    store,
    deadlineMs: scene.emulator.now() + HOUR_MS,
    now: scene.clock.now,
    sleep: scene.clock.sleep,
    log: (line) => lines.push(line),
    ...options,
  };
  const program =
    kind === "Deposit"
      ? submitDepositProgram(
          user.view,
          scene.contracts,
          {
            ...depositConfig(scene, user as User, amount),
            ...config,
          } as SubmitDepositConfig,
          runOptions,
        )
      : submitWithdrawalProgram(
          user.view,
          scene.contracts,
          {
            ...withdrawalConfig(scene, user.account, amount),
            ...config,
          } as SubmitWithdrawalConfig,
          runOptions,
        );
  return { result: await Effect.runPromise(Effect.either(program)), lines };
};

export const landed = (run: RunResult): UserSubmissionOutcome => {
  if (run.result._tag === "Left")
    throw new Error(
      `expected the submission to land: ${run.result.left.message}\n${run.lines.join("\n")}`,
    );
  return run.result.right;
};

export const refusal = (run: RunResult): UserSubmissionError => {
  if (
    run.result._tag === "Right" ||
    !(run.result.left instanceof UserSubmissionError)
  )
    throw new Error(
      `expected a refusal, got ${run.result._tag === "Right" ? "an outcome" : run.result.left.message}`,
    );
  return run.result.left;
};

export const statusOf = async (scene: UserHistoryScene, txHash: string) =>
  (await scene.emulator.getTransactionStatus(txHash)).status;

export const listFor = (scene: UserHistoryScene, kind: Kind) =>
  kind === "Deposit" ? scene.pair.deposit.list : scene.pair.withdrawal.list;

/** How many Order nodes the list holds for this event id. */
export const ordersFor = async (
  scene: UserHistoryScene,
  kind: Kind,
  nonce: OutRef,
): Promise<number> => {
  const list = listFor(scene, kind);
  const key = await Effect.runPromise(
    eventHistoryKey({
      transactionId: nonce.txHash,
      outputIndex: BigInt(nonce.outputIndex),
    }),
  );
  return (await scene.operator.utxosAt(list.spendingScriptAddress)).filter(
    (utxo) => utxo.assets[list.policyId + key] === 1n,
  ).length;
};

export const outRefLabels = (utxos: readonly UTxO[]) =>
  utxos.map((utxo) => `${utxo.txHash}#${String(utxo.outputIndex)}`).sort();
