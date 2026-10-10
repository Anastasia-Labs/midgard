import { execFile } from "node:child_process";
import { existsSync, mkdtempSync, readFileSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

import * as SDK from "@al-ft/midgard-sdk";
import {
  generateEmulatorAccountFromPrivateKey,
  generateSeedPhrase,
  type LucidEvolution,
  paymentCredentialOf,
  PROTOCOL_PARAMETERS_DEFAULT,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it, vi } from "vitest";

import {
  daBondAssembleCommand,
  daBondChainSubmit,
  type DaBondContext,
  type DaBondL1Access,
  daBondLucid,
  DaBondSlotMappingError,
  daBondStatusCommand,
  daBondSubmitAndConfirm,
  daBondTopUpCommand,
  daBondWithdrawBuildCommand,
  loadDaBondContext,
} from "../src/commands/da-bond.js";
import {
  daBondSigningKeyFromSecret,
  type DaBondUnsignedFile,
  runDaBondWitnessCommand,
} from "../src/commands/da-bond-files.js";
import {
  AVAILABILITY_DEFAULT_POOL_LOVELACE,
  type AvailabilityFixture,
  createAvailabilityFixture,
} from "./helpers/availability-challenge-emulator.js";

/**
 * `loadDaBondContext` on a Custom manifest: a finalized Custom manifest cannot
 * pass this suite's compiled (Preprod) profile check, so while `custom.active`
 * is set the manifest read and verification and the first reference
 * authentication (the stage right after Lucid is built) are stubbed, the L1
 * access is a fake (`fakeAccess`),
 * and the Lucid instance that reaches that stage is captured. With
 * `completeLoad` set, the stubbed manifest also names the DA params governor
 * and the reference authentication answers with the given pool references, so
 * the context loads in full. Otherwise every mocked export is the real one.
 */
const custom = vi.hoisted(() => ({
  active: false,
  reached: [] as unknown[],
  completeLoad: undefined as
    | undefined
    | {
        readonly references: Readonly<Record<string, unknown>>;
        readonly parameters: unknown;
      },
}));

vi.mock("../src/commands/contract-deployment-info.js", async (original) => {
  const actual =
    await original<
      typeof import("../src/commands/contract-deployment-info.js")
    >();
  return {
    ...actual,
    readDeploymentManifestFile: (path: string) =>
      custom.active
        ? {
            network: "Custom",
            manifestId: "custom-manifest",
            ...(custom.completeLoad === undefined
              ? {}
              : {
                  contracts: {
                    daParamsGovernorSpend: { scriptHash: "aa".repeat(28) },
                    daParamsGovernorMint: { scriptHash: "bb".repeat(28) },
                  },
                  deploymentProfile: {
                    timing: { da_bond_withdraw_delay_ms: 60_000 },
                  },
                }),
          }
        : actual.readDeploymentManifestFile(path),
  };
});

vi.mock(
  "@al-ft/midgard-core/deployment-manifest-identity",
  async (original) => {
    const actual =
      await original<
        typeof import("@al-ft/midgard-core/deployment-manifest-identity")
      >();
    return {
      ...actual,
      verifyFinalizedDeploymentManifest: (
        ...args: Parameters<typeof actual.verifyFinalizedDeploymentManifest>
      ) =>
        custom.active
          ? args[0]
          : actual.verifyFinalizedDeploymentManifest(...args),
    };
  },
);

vi.mock(
  "../src/commands/availability-challenge-deployment.js",
  async (original) => {
    const actual =
      await original<
        typeof import("../src/commands/availability-challenge-deployment.js")
      >();
    return {
      ...actual,
      manifestReferenceScriptAuthPolicy: (
        ...args: Parameters<typeof actual.manifestReferenceScriptAuthPolicy>
      ) =>
        custom.active
          ? "stub-policy"
          : actual.manifestReferenceScriptAuthPolicy(...args),
      authenticatedManifestReference: (
        ...args: Parameters<typeof actual.authenticatedManifestReference>
      ) => {
        if (!custom.active)
          return actual.authenticatedManifestReference(...args);
        custom.reached.push(args[0]);
        if (custom.completeLoad !== undefined)
          return Promise.resolve(custom.completeLoad.references[args[3]]);
        return Promise.reject(new Error("stop after Lucid is built"));
      },
      availabilityParametersFromManifest: (
        ...args: Parameters<typeof actual.availabilityParametersFromManifest>
      ) =>
        custom.active && custom.completeLoad !== undefined
          ? custom.completeLoad.parameters
          : actual.availabilityParametersFromManifest(...args),
    };
  },
);

/**
 * Contract tests for the operator `da-bond` CLI (#691 AC1, AC2) on the real
 * pool, DA params governor and emulator ledger: one test per command, each
 * refusal paired with its honest control. The owners are the fixture's
 * challenger and responder, `update_threshold` 2. Validator-level refusals
 * are covered by `da-bond-pool-lifecycle.test.ts`; these assert what the CLI
 * prints, writes and submits.
 */

const TIMEOUT_MS = 600_000;
const execFileAsync = promisify(execFile);
const NODE_DIST = fileURLToPath(new URL("../dist/index.js", import.meta.url));

let f: AvailabilityFixture;
let dir: string;
let submitted: ReturnType<typeof vi.fn<(cbor: string) => Promise<string>>>;
let ctx: DaBondContext;
let fileCount = 0;

const file = (name: string): string => {
  fileCount += 1;
  return join(dir, `${fileCount.toString()}-${name}.json`);
};

const readUnsigned = (path: string): DaBondUnsignedFile =>
  JSON.parse(readFileSync(path, "utf8")) as DaBondUnsignedFile;

const ownersCsv = () => f.daParamsDatum.owners.join(",");

/** An owner witnesses in-process, with only their key in a one-var env. */
const witnessInProcess = async (
  unsignedPath: string,
  privateKey: string,
): Promise<string> => {
  const out = file("witness");
  await runDaBondWitnessCommand(
    unsignedPath,
    { keyEnv: "OWNER_KEY", out },
    { OWNER_KEY: privateKey },
  );
  return out;
};

/**
 * An owner witnesses in a separate process of the built CLI whose
 * environment holds only `PATH` and that owner's key.
 */
const witnessInChild = async (
  unsignedPath: string,
  privateKey: string,
): Promise<{ out: string; stdout: string }> => {
  const out = file("child-witness");
  const { stdout } = await execFileAsync(
    process.execPath,
    [
      NODE_DIST,
      "da-bond",
      "witness",
      "--key-env",
      "OWNER_KEY",
      unsignedPath,
      "--out",
      out,
    ],
    {
      cwd: dir,
      env: { PATH: process.env.PATH ?? "", OWNER_KEY: privateKey },
      encoding: "utf8",
    },
  );
  return { out, stdout };
};

const buildWithdraw = (
  step: "begin" | "cancel" | "complete",
  extra: { validForMs?: string; amount?: string; to?: string } = {},
) => {
  const buildUnsigned = file(`unsigned-${step}`);
  return daBondWithdrawBuildCommand(ctx, step, {
    feeAddress: f.responder.address,
    signers: ownersCsv(),
    buildUnsigned,
    ...extra,
  }).then((result) => ({ result, buildUnsigned }));
};

/** Build, witness with both owners in-process, assemble. */
const runQuorumStep = async (
  step: "begin" | "cancel" | "complete",
  extra: { validForMs?: string; amount?: string; to?: string } = {},
) => {
  const { buildUnsigned } = await buildWithdraw(step, extra);
  const witnesses = [
    await witnessInProcess(buildUnsigned, f.responder.privateKey),
    await witnessInProcess(buildUnsigned, f.challenger.privateKey),
  ];
  return daBondAssembleCommand(ctx, buildUnsigned, witnesses);
};

/**
 * `ctx` with Kupo failing every pool read once `submit` has landed a
 * transaction, as when it stops answering right after confirmation.
 */
const readsFailAfterSubmit = () => {
  let landed: string | undefined;
  let readsFail = false;
  const lucid = new Proxy(f.lucid, {
    get(target, property) {
      if (property === "utxosAtWithUnit" && readsFail)
        return async () => {
          throw new Error("kupo getTransactionStatus failed (HTTP 503)");
        };
      const value = Reflect.get(target, property, target);
      return typeof value === "function" ? value.bind(target) : value;
    },
  });
  const failing: DaBondContext = {
    ...ctx,
    lucid,
    submit: async (cbor) => {
      landed = await submitted(cbor);
      readsFail = true;
      return landed;
    },
  };
  return { ctx: failing, landed: () => landed };
};

const poolOutRef = async () => {
  const pool = await f.getPool();
  return `${pool.txHash}#${pool.outputIndex.toString()}`;
};

beforeAll(async () => {
  f = await createAvailabilityFixture();
  dir = mkdtempSync(join(tmpdir(), "midgard-da-bond-cli-"));
  submitted = vi.fn(async (cbor: string) => {
    const txHash = await f.emulator.submitTx(cbor);
    f.emulator.awaitBlock(1);
    return txHash;
  });
  ctx = {
    lucid: f.lucid,
    network: "Preprod",
    manifestId: "emulator-da-bond-cli",
    poolValidator: f.contracts.daBondPool,
    poolSpendingReference: f.poolReferences.daBondPoolSpending,
    parameters: f.parameters,
    daParamsGovernor: {
      address: f.contracts.daParamsGovernor.spendingScriptAddress,
      unit: SDK.daParamsUnit(f.contracts.daParamsGovernor),
    },
    withdrawDelayMs: f.timing.daBondWithdrawDelayMs,
    now: () => f.emulator.now(),
    submit: submitted,
  };
}, TIMEOUT_MS);

describe("da-bond CLI on an emulator pool", { shuffle: false }, () => {
  it(
    "status reads a full Bonded pool",
    async () => {
      expect(await daBondStatusCommand(ctx)).toEqual({
        poolOutRef: await poolOutRef(),
        state: "bonded",
        lovelace: AVAILABILITY_DEFAULT_POOL_LOVELACE.toString(),
        backing: (2n * f.parameters.da_bond_lovelace).toString(),
        requiredBacking: f.parameters.da_bond_lovelace.toString(),
        belowBond: false,
      });
    },
    TIMEOUT_MS,
  );

  it(
    "top-up adds the amount; a below-minimum top-up is refused and submits nothing",
    async () => {
      const minimum = f.parameters.da_bond_min_top_up_lovelace;
      const before = await poolOutRef();
      await expect(
        daBondTopUpCommand(ctx, {
          amount: (minimum - 1n).toString(),
          walletSecret: f.responder.privateKey,
        }),
      ).rejects.toThrow(
        `Refusing top-up: ${(minimum - 1n).toString()} lovelace is below da_bond_min_top_up_lovelace ${minimum.toString()}; nothing was submitted`,
      );
      expect(submitted).not.toHaveBeenCalled();
      expect(await poolOutRef()).toBe(before);

      const result = await daBondTopUpCommand(ctx, {
        amount: minimum.toString(),
        walletSecret: f.responder.privateKey,
      });
      expect(submitted).toHaveBeenCalledTimes(1);
      expect(result.previousPoolOutRef).toBe(before);
      expect(result.status.poolOutRef).toBe(`${result.txHash}#0`);
      expect(await poolOutRef()).toBe(result.status.poolOutRef);
      expect(result.status.lovelace).toBe(
        (AVAILABILITY_DEFAULT_POOL_LOVELACE + minimum).toString(),
      );
      expect(result.status.state).toBe("bonded");
    },
    TIMEOUT_MS,
  );

  it(
    "top-up from a mnemonic funds from the node operator wallet's base address",
    async () => {
      const seed = generateSeedPhrase();
      f.lucid.selectWallet.fromSeed(seed);
      const operatorAddress = await f.lucid.wallet().address();
      f.lucid.selectWallet.fromPrivateKey(f.responder.privateKey);
      const funding = await f.lucid
        .newTx()
        .pay.ToAddress(operatorAddress, { lovelace: 50_000_000n })
        .complete();
      await submitted((await funding.sign.withWallet().complete()).toCBOR());
      submitted.mockClear();
      const before = BigInt((await daBondStatusCommand(ctx)).lovelace);
      const minimum = f.parameters.da_bond_min_top_up_lovelace;
      const result = await daBondTopUpCommand(ctx, {
        amount: minimum.toString(),
        walletSecret: seed,
      });
      expect(submitted).toHaveBeenCalledTimes(1);
      expect(result.status.lovelace).toBe((before + minimum).toString());
      expect(
        (await f.lucid.utxosAt(operatorAddress)).some(
          (utxo) => utxo.txHash === result.txHash,
        ),
      ).toBe(true);
    },
    TIMEOUT_MS,
  );

  let begin: { unsigned: string; responderWitness: string } | undefined;

  it(
    "withdraw begin: owners witness in separate processes; one of two witnesses is refused and submits nothing",
    async () => {
      expect(
        existsSync(NODE_DIST),
        `${NODE_DIST} is missing: run pnpm --dir demo/midgard-node run build`,
      ).toBe(true);
      submitted.mockClear();
      const { result, buildUnsigned } = await buildWithdraw("begin");
      const unsigned = readUnsigned(buildUnsigned);
      expect(unsigned.format).toBe("midgard-da-bond-unsigned-v1");
      expect(unsigned.action).toBe("BeginWithdraw");
      expect(unsigned.requiredSigners).toEqual(
        [...f.daParamsDatum.owners].sort(),
      );
      expect(unsigned.feePayerKeyHash).toBe(f.responderKey);
      expect(unsigned.poolOutRef).toBe(await poolOutRef());
      expect(unsigned.txBodyHash).toBe(
        SDK.daBondPoolTxBodyHash(unsigned.txCbor),
      );
      expect(result.submitted).toBe(false);
      expect(result.unlockAt).toBe(
        (
          BigInt(unsigned.validTo!) -
          1n +
          f.timing.daBondWithdrawDelayMs
        ).toString(),
      );
      expect(submitted).not.toHaveBeenCalled();

      const responder = await witnessInChild(
        buildUnsigned,
        f.responder.privateKey,
      );
      const challenger = await witnessInChild(
        buildUnsigned,
        f.challenger.privateKey,
      );
      expect(JSON.parse(responder.stdout)).toMatchObject({
        keyHash: f.responderKey,
        txBodyHash: unsigned.txBodyHash,
        roles: ["required-signer", "fee-payer"],
      });
      expect(JSON.parse(challenger.stdout)).toMatchObject({
        action: "BeginWithdraw",
        keyHash: f.challengerKey,
        roles: ["required-signer"],
        outputs: expect.arrayContaining([
          {
            address: f.contracts.daBondPool.spendingScriptAddress,
            lovelace: (await f.getPool()).assets.lovelace!.toString(),
          },
        ]),
      });
      expect(JSON.parse(challenger.stdout)).not.toHaveProperty("amount");

      const before = await poolOutRef();
      await expect(
        daBondAssembleCommand(ctx, buildUnsigned, [responder.out]),
      ).rejects.toThrow(new Error(SDK.daBondPoolQuorumShortfallMessage(1, 2n)));
      expect(submitted).not.toHaveBeenCalled();
      expect(await poolOutRef()).toBe(before);
      begin = { unsigned: buildUnsigned, responderWitness: responder.out };

      const assembled = await daBondAssembleCommand(ctx, buildUnsigned, [
        responder.out,
        challenger.out,
      ]);
      expect(submitted).toHaveBeenCalledTimes(1);
      expect(assembled.txHash).toBe(unsigned.txBodyHash);
      expect(assembled.ownerWitnesses).toEqual(
        [...f.daParamsDatum.owners].sort(),
      );
      const status = await daBondStatusCommand(ctx);
      expect(status).toMatchObject({
        state: "withdrawing",
        belowBond: false,
        unlockAt: result.unlockAt,
        unlockAtIso: new Date(Number(result.unlockAt)).toISOString(),
        unlockable: false,
      });
    },
    TIMEOUT_MS,
  );

  it(
    "witness refuses a key that is neither a required signer nor the fee payer",
    async () => {
      const outsider = generateEmulatorAccountFromPrivateKey({});
      const outsiderKey = paymentCredentialOf(outsider.address).hash;
      await expect(
        runDaBondWitnessCommand(
          begin!.unsigned,
          { keyEnv: "OWNER_KEY", out: file("outsider") },
          { OWNER_KEY: outsider.privateKey },
        ),
      ).rejects.toThrow(
        `Refusing to witness: key ${outsiderKey} is neither a required signer`,
      );
    },
    TIMEOUT_MS,
  );

  it(
    "witness and assemble refuse a file whose action is not its transaction's pool redeemer",
    async () => {
      submitted.mockClear();
      const tampered = file("tampered-action");
      writeFileSync(
        tampered,
        JSON.stringify({
          ...readUnsigned(begin!.unsigned),
          action: "CancelWithdraw",
        }),
      );
      const refusal =
        "action CancelWithdraw is not the transaction's pool redeemer BeginWithdraw";
      await expect(
        runDaBondWitnessCommand(
          tampered,
          { keyEnv: "OWNER_KEY", out: file("tampered-witness") },
          { OWNER_KEY: f.challenger.privateKey },
        ),
      ).rejects.toThrow(refusal);
      await expect(
        daBondAssembleCommand(ctx, tampered, [begin!.responderWitness]),
      ).rejects.toThrow(refusal);
      expect(submitted).not.toHaveBeenCalled();
    },
    TIMEOUT_MS,
  );

  it(
    "assemble refuses a spent pool input and a witness for another transaction",
    async () => {
      submitted.mockClear();
      const stale = readUnsigned(begin!.unsigned);
      const challengerAgain = await witnessInProcess(
        begin!.unsigned,
        f.challenger.privateKey,
      );
      await expect(
        daBondAssembleCommand(ctx, begin!.unsigned, [
          begin!.responderWitness,
          challengerAgain,
        ]),
      ).rejects.toThrow(
        `Refusing to assemble: the pool input ${stale.poolOutRef} is no longer live; rebuild the unsigned transaction`,
      );
      const { buildUnsigned } = await buildWithdraw("cancel");
      await expect(
        daBondAssembleCommand(ctx, buildUnsigned, [begin!.responderWitness]),
      ).rejects.toThrow(
        `is for transaction ${stale.txBodyHash}, not ${readUnsigned(buildUnsigned).txBodyHash}`,
      );
      expect(submitted).not.toHaveBeenCalled();
    },
    TIMEOUT_MS,
  );

  it(
    "withdraw complete before unlock_at is refused with unlock_at and the lower bound",
    async () => {
      const pool = SDK.decodeDaBondPoolDatum((await f.getPool()).datum!);
      if (pool === "Bonded") throw new Error("expected a Withdrawing pool");
      const unlockAt = pool.Withdrawing.unlock_at;
      // An operator clock half a slot ahead: the refusal reports the lower
      // bound the SDK compared, moved up to the next slot boundary.
      const midSlot = { ...ctx, now: () => f.emulator.now() + 500 };
      const lower = SDK.slotAlignedLowerBoundAtOrAfter(
        f.lucid,
        BigInt(midSlot.now()),
      );
      expect(lower).toBe(BigInt(f.emulator.now() + 1_000));
      const buildUnsigned = file("early-complete");
      await expect(
        daBondWithdrawBuildCommand(midSlot, "complete", {
          feeAddress: f.responder.address,
          signers: ownersCsv(),
          buildUnsigned,
          amount: "1000000",
          to: f.responder.address,
        }),
      ).rejects.toThrow(
        `Refusing withdraw complete: the pool unlocks at unlock_at ${unlockAt.toString()} (${new Date(Number(unlockAt)).toISOString()}), after this transaction's lower bound ${lower.toString()} (${new Date(Number(lower)).toISOString()}); retry at or after unlock_at`,
      );
      expect(existsSync(buildUnsigned)).toBe(false);
    },
    TIMEOUT_MS,
  );

  it(
    "withdraw cancel returns the pool to Bonded",
    async () => {
      const result = await runQuorumStep("cancel");
      expect(result.action).toBe("CancelWithdraw");
      expect(result.status).toMatchObject({
        state: "bonded",
        belowBond: false,
      });
      expect(result.status).not.toHaveProperty("unlockAt");
    },
    TIMEOUT_MS,
  );

  it(
    "assemble refuses a transaction whose validity upper bound has passed",
    async () => {
      submitted.mockClear();
      const { buildUnsigned } = await buildWithdraw("begin", {
        validForMs: "60000",
      });
      const witnesses = [
        await witnessInProcess(buildUnsigned, f.responder.privateKey),
        await witnessInProcess(buildUnsigned, f.challenger.privateKey),
      ];
      const validTo = BigInt(readUnsigned(buildUnsigned).validTo!);
      f.advanceToMs(validTo);
      await expect(
        daBondAssembleCommand(ctx, buildUnsigned, witnesses),
      ).rejects.toThrow(
        `Refusing to assemble: the transaction's validity ended at ${validTo.toString()} (${new Date(Number(validTo)).toISOString()}); rebuild the unsigned transaction`,
      );
      expect(submitted).not.toHaveBeenCalled();
      expect((await daBondStatusCommand(ctx)).state).toBe("bonded");
    },
    TIMEOUT_MS,
  );

  it(
    "withdraw complete after unlock_at pays --to exactly the amount; status then reads a drained pool",
    async () => {
      const begun = await runQuorumStep("begin");
      const unlockAt = BigInt(begun.status.unlockAt!);
      f.advanceToMs(unlockAt);
      expect((await daBondStatusCommand(ctx)).unlockable).toBe(true);
      const bond = f.parameters.da_bond_lovelace;
      const backing = BigInt(begun.status.backing);
      const amount = backing - bond / 2n;
      const recipient = generateEmulatorAccountFromPrivateKey({}).address;
      const { buildUnsigned } = await buildWithdraw("complete", {
        amount: amount.toString(),
        to: recipient,
      });
      const responderWitness = file("complete-responder");
      const shown = await runDaBondWitnessCommand(
        buildUnsigned,
        { keyEnv: "OWNER_KEY", out: responderWitness },
        { OWNER_KEY: f.responder.privateKey },
      );
      // The owner is shown what the transaction pays, read from its CBOR.
      expect(shown).toMatchObject({
        action: "CompleteWithdraw",
        amount: amount.toString(),
        outputs: expect.arrayContaining([
          { address: recipient, lovelace: amount.toString() },
          {
            address: f.contracts.daBondPool.spendingScriptAddress,
            lovelace: (BigInt(begun.status.lovelace) - amount).toString(),
          },
        ]),
      });
      const completed = await daBondAssembleCommand(ctx, buildUnsigned, [
        responderWitness,
        await witnessInProcess(buildUnsigned, f.challenger.privateKey),
      ]);
      expect(completed.action).toBe("CompleteWithdraw");
      const paid = await f.lucid.utxosAt(recipient);
      expect(paid).toHaveLength(1);
      expect(paid[0]!.assets).toEqual({ lovelace: amount });
      expect(paid[0]!.txHash).toBe(completed.txHash);
      expect(await daBondStatusCommand(ctx)).toEqual({
        poolOutRef: await poolOutRef(),
        state: "bonded",
        lovelace: (BigInt(begun.status.lovelace) - amount).toString(),
        backing: (bond / 2n).toString(),
        requiredBacking: bond.toString(),
        belowBond: true,
      });
    },
    TIMEOUT_MS,
  );

  it(
    "a top-up whose status read fails after it landed names the submitted transaction",
    async () => {
      const readsFail = readsFailAfterSubmit();
      const before = await poolOutRef();
      const error = await daBondTopUpCommand(readsFail.ctx, {
        amount: f.parameters.da_bond_min_top_up_lovelace.toString(),
        walletSecret: f.responder.privateKey,
      }).catch((caught: unknown) => caught);
      const landed = readsFail.landed();
      expect(landed).toMatch(/^[0-9a-f]{64}$/u);
      expect(await poolOutRef()).toBe(`${landed!}#0`);
      expect(await poolOutRef()).not.toBe(before);
      // Fatal (the CLI's failCli exits 1), and it says the transaction is
      // confirmed rather than asking whether it landed.
      expect(error).toBeInstanceOf(Error);
      expect((error as Error).message).toBe(
        `Transaction ${landed!} is confirmed, but reading the pool status failed: kupo getTransactionStatus failed (HTTP 503); do not submit it again`,
      );
    },
    TIMEOUT_MS,
  );

  it(
    "an assembled withdraw step whose status read fails after it landed names the confirmed transaction",
    async () => {
      const { buildUnsigned } = await buildWithdraw("begin");
      const witnesses = [
        await witnessInProcess(buildUnsigned, f.responder.privateKey),
        await witnessInProcess(buildUnsigned, f.challenger.privateKey),
      ];
      const readsFail = readsFailAfterSubmit();
      const error = await daBondAssembleCommand(
        readsFail.ctx,
        buildUnsigned,
        witnesses,
      ).catch((caught: unknown) => caught);
      const landed = readsFail.landed();
      expect(landed).toBe(readUnsigned(buildUnsigned).txBodyHash);
      expect(await poolOutRef()).toBe(`${landed!}#0`);
      expect((await daBondStatusCommand(ctx)).state).toBe("withdrawing");
      expect(error).toBeInstanceOf(Error);
      expect((error as Error).message).toBe(
        `Transaction ${landed!} is confirmed, but reading the pool status failed: kupo getTransactionStatus failed (HTTP 503); do not submit it again`,
      );
    },
    TIMEOUT_MS,
  );
});

describe("da-bond production submit", () => {
  it("names the accepted transaction when the confirmation wait fails", async () => {
    const txHash = "ab".repeat(32);
    const submit = daBondSubmitAndConfirm(
      async () => txHash,
      async () => {
        // A Kupo poll error aborts lucid's wait with no hash in the message.
        throw new Error("kupo getTransactionStatus failed (HTTP 503)");
      },
    );
    await expect(submit("84a0")).rejects.toThrow(
      `Transaction ${txHash} was submitted, but waiting for its confirmation failed: kupo getTransactionStatus failed (HTTP 503). Check whether ${txHash} landed before retrying; do not submit it again`,
    );
    await expect(
      daBondSubmitAndConfirm(
        async () => txHash,
        async () => undefined,
      )("84a0"),
    ).resolves.toBe(txHash);
  });

  it("the chain submit loadDaBondContext installs names the accepted transaction when lucid's confirmation wait fails", async () => {
    const txHash = "cd".repeat(32);
    const sent: string[] = [];
    const provider = {
      submitTx: async (txCbor: string) => {
        sent.push(txCbor);
        return txHash;
      },
    };
    const waited: string[] = [];
    // A live (non-emulator) provider: only the exact-status wait runs.
    const lucid = (fails: boolean) =>
      ({
        config: () => ({ provider }),
        awaitTxConfirmation: async (hash: string) => {
          waited.push(hash);
          if (fails)
            throw new Error("kupo getTransactionStatus failed (HTTP 503)");
          return true;
        },
      }) as unknown as LucidEvolution;
    await expect(
      daBondChainSubmit(provider, lucid(true))("84a0"),
    ).rejects.toThrow(
      `Transaction ${txHash} was submitted, but waiting for its confirmation failed: kupo getTransactionStatus failed (HTTP 503). Check whether ${txHash} landed before retrying; do not submit it again`,
    );
    await expect(
      daBondChainSubmit(provider, lucid(false))("84a1"),
    ).resolves.toBe(txHash);
    expect(sent).toEqual(["84a0", "84a1"]);
    expect(waited).toEqual([txHash, txHash]);
  });
});

/**
 * The node's L1 access as the da-bond commands read it: the ledger's slot
 * mapping and tip, over a provider that only answers what `Lucid(...)` asks
 * when it is built. The chain started at `genesisStartMs` at one-second slots.
 */
const fakeAccess = (options: {
  genesisStartMs: number;
  slotConfigFails?: true;
  /** How many slots the ledger tip answers behind the wall clock. */
  tipLagSlots?: number;
}) => {
  const reads: string[] = [];
  /** Every slot the ledger tip answered, in order. */
  const tipSlots: number[] = [];
  const getProtocolParameters = vi.fn(async () => PROTOCOL_PARAMETERS_DEFAULT);
  const access: DaBondL1Access = {
    provider: {
      getProtocolParameters,
    } as unknown as DaBondL1Access["provider"],
    endpoint: "/run/cardano/node.socket",
    slotConfig: async () => {
      reads.push("slotConfig");
      if (options.slotConfigFails !== undefined)
        throw new Error("L1 provider transport unavailable: node_unreachable");
      return {
        zeroTime: options.genesisStartMs,
        zeroSlot: 0,
        slotLength: 1_000,
      };
    },
    ledgerTip: async () => {
      reads.push("ledgerTip");
      const slot =
        Math.floor((Date.now() - options.genesisStartMs) / 1_000) -
        (options.tipLagSlots ?? 0);
      tipSlots.push(slot);
      return { slot, id: "ab".repeat(32) };
    },
  };
  return { access, reads, tipSlots, getProtocolParameters };
};

describe("da-bond on the ledger's slot mapping (P25)", () => {
  // A whole second, about an hour before now.
  const genesisStartMs = Math.floor(Date.now() / 1_000) * 1_000 - 3_600_000;

  it("builds a Custom Lucid with the slot mapping the local node's ledger gives", async () => {
    const { access, reads, getProtocolParameters } = fakeAccess({
      genesisStartMs,
    });
    const lucid = await daBondLucid({ access, network: "Custom" });
    expect(lucid.config().network).toBe("Custom");
    expect(lucid.config().slotConfig).toEqual({
      zeroTime: genesisStartMs,
      zeroSlot: 0,
      slotLength: 1_000,
    });
    expect(reads).toEqual(["slotConfig"]);
    expect(getProtocolParameters).toHaveBeenCalled();
  });

  it("builds a public network's Lucid with the ledger's slot mapping too", async () => {
    const { access } = fakeAccess({ genesisStartMs });
    const lucid = await daBondLucid({ access, network: "Preprod" });
    expect(lucid.config().network).toBe("Preprod");
    expect(lucid.config().slotConfig).toEqual({
      zeroTime: genesisStartMs,
      zeroSlot: 0,
      slotLength: 1_000,
    });
  });

  it("refuses a deployment whose ledger slot mapping read fails, with a named error and before Lucid is built", async () => {
    const { access, getProtocolParameters } = fakeAccess({
      genesisStartMs,
      slotConfigFails: true,
    });
    const refused = daBondLucid({ access, network: "Custom" });
    await expect(refused).rejects.toBeInstanceOf(DaBondSlotMappingError);
    await expect(refused).rejects.toThrow(
      "Refusing the deployment: the slot mapping from the L1 access at /run/cardano/node.socket failed: L1 provider transport unavailable: node_unreachable",
    );
    expect(getProtocolParameters).not.toHaveBeenCalled();
  });

  const loadCustom = async (access: DaBondL1Access) => {
    custom.active = true;
    custom.reached = [];
    try {
      const loaded = await loadDaBondContext(
        { manifest: "custom-manifest.json" },
        access,
      ).then(
        () => undefined,
        (error: unknown) => error,
      );
      return { loaded, reached: custom.reached };
    } finally {
      custom.active = false;
    }
  };

  it("loadDaBondContext admits a Custom manifest and builds its Lucid with the ledger's slot mapping", async () => {
    const { access, reads } = fakeAccess({ genesisStartMs });
    const { loaded, reached } = await loadCustom(access);
    // It gets past the network parser and the manifest/Lucid network check
    // to the reference authentication, with the ledger mapping installed.
    expect(loaded).toEqual(new Error("stop after Lucid is built"));
    expect(reached).toHaveLength(1);
    const lucid = reached[0] as LucidEvolution;
    expect(lucid.config().network).toBe("Custom");
    expect(lucid.config().slotConfig).toEqual({
      zeroTime: genesisStartMs,
      zeroSlot: 0,
      slotLength: 1_000,
    });
    expect(reads).toEqual(["slotConfig"]);
  });

  it("loadDaBondContext reads the clock from the local node's ledger tip, which trails the wall clock", async () => {
    // The node checks a lower bound against its tip slot + 1. A withdraw
    // begin or complete whose lower bound came from the wall clock, 30 slots
    // ahead of this tip, would be refused as not yet valid.
    const { access, tipSlots } = fakeAccess({
      genesisStartMs,
      tipLagSlots: 30,
    });
    custom.active = true;
    custom.reached = [];
    custom.completeLoad = {
      references: {
        daBondPoolSpend: f.poolReferences.daBondPoolSpending,
        daBondPoolMint: f.poolReferences.daBondPoolMinting,
      },
      parameters: f.parameters,
    };
    try {
      const loaded = await loadDaBondContext(
        { manifest: "custom-manifest.json" },
        access,
      );
      const tipSlot = tipSlots.at(-1)!;
      expect(loaded.now()).toBe(genesisStartMs + tipSlot * 1_000);
      expect(loaded.now()).toBeLessThanOrEqual(Date.now() - 29_000);
    } finally {
      custom.active = false;
      custom.completeLoad = undefined;
    }
  });

  it("loadDaBondContext refuses a manifest whose ledger slot mapping read fails, before Lucid is built", async () => {
    const { access, getProtocolParameters } = fakeAccess({
      genesisStartMs,
      slotConfigFails: true,
    });
    const { loaded, reached } = await loadCustom(access);
    expect(loaded).toBeInstanceOf(DaBondSlotMappingError);
    expect((loaded as Error).message).toContain(
      "Refusing the deployment: the slot mapping from the L1 access at /run/cardano/node.socket failed",
    );
    expect(getProtocolParameters).not.toHaveBeenCalled();
    expect(reached).toEqual([]);
  });
});

describe("da-bond witness signing secret", () => {
  it("derives a mnemonic's enterprise payment key and passes a bech32 key through", () => {
    const seed = generateSeedPhrase();
    const wallet = walletFromSeed(seed, {
      addressType: "Enterprise",
      accountIndex: 0,
      network: "Preprod",
    });
    expect(daBondSigningKeyFromSecret(seed)).toBe(wallet.paymentKey);
    expect(daBondSigningKeyFromSecret(` ${wallet.paymentKey}\n`)).toBe(
      wallet.paymentKey,
    );
    expect(() => daBondSigningKeyFromSecret("not a key")).toThrow(
      "neither a bech32 ed25519_sk/ed25519e_sk key nor a mnemonic",
    );
  });
});
