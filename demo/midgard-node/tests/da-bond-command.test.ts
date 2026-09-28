import { execFile } from "node:child_process";
import { existsSync, mkdtempSync, readFileSync, writeFileSync } from "node:fs";
import { createServer } from "node:http";
import type { AddressInfo } from "node:net";
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
  type Provider,
  SLOT_CONFIG_NETWORK,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it, vi } from "vitest";

import {
  daBondAssembleCommand,
  daBondChainSubmit,
  type DaBondContext,
  DaBondCustomSlotMappingError,
  daBondLucid,
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
 * is set the manifest read and verification, the provider, and the first
 * reference authentication (the stage right after Lucid is built) are stubbed,
 * and the Lucid instance that reaches that stage is captured. Otherwise every
 * mocked export is the real one.
 */
const custom = vi.hoisted(() => ({
  active: false,
  provider: undefined as unknown,
  reached: [] as unknown[],
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
        ? { network: "Custom", manifestId: "custom-manifest" }
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

vi.mock("../src/services/native-ledger.js", async (original) => {
  const actual =
    await original<typeof import("../src/services/native-ledger.js")>();
  return {
    ...actual,
    makeNodeKupmios: (...args: Parameters<typeof actual.makeNodeKupmios>) =>
      custom.active ? custom.provider : actual.makeNodeKupmios(...args),
  };
});

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
        return Promise.reject(new Error("stop after Lucid is built"));
      },
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

describe("da-bond CLI on an emulator pool", () => {
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
 * A local Ogmios answering the three HTTP queries the node's `Custom` slot
 * mapping makes: `/health`, `queryNetwork/tip` and the Shelley genesis. The
 * chain started `genesisStartMs` ago-ish at one-second slots, so the live
 * snapshot and the genesis agree.
 */
const fakeOgmios = async (options: {
  genesisStartMs: number;
  genesisFails?: true;
}) => {
  const requests: string[] = [];
  const slotNow = () =>
    Math.floor((Date.now() - options.genesisStartMs) / 1_000);
  const server = createServer((request, response) => {
    let body = "";
    request.on("data", (chunk: Buffer) => {
      body += chunk.toString("utf8");
    });
    request.on("end", () => {
      const reply = (status: number, payload: unknown) => {
        response.writeHead(status, { "content-type": "application/json" });
        response.end(JSON.stringify(payload));
      };
      if (request.method === "GET" && request.url === "/health") {
        requests.push("health");
        reply(200, {
          connectionStatus: "connected",
          networkSynchronization: 1,
          lastKnownTip: { slot: slotNow() },
          lastTipUpdate: new Date().toISOString(),
        });
        return;
      }
      const { method, id } = JSON.parse(body) as {
        method: string;
        id: string;
      };
      requests.push(method);
      if (method === "queryNetwork/tip") {
        reply(200, { jsonrpc: "2.0", result: { slot: slotNow() }, id });
      } else if (
        method === "queryNetwork/genesisConfiguration" &&
        options.genesisFails === undefined
      ) {
        reply(200, {
          jsonrpc: "2.0",
          result: {
            startTime: new Date(options.genesisStartMs).toISOString(),
            slotLength: { milliseconds: 1_000 },
          },
          id,
        });
      } else {
        reply(500, { error: `no answer for ${method}` });
      }
    });
  });
  await new Promise<void>((resolve) =>
    server.listen(0, "127.0.0.1", () => resolve()),
  );
  const { port } = server.address() as AddressInfo;
  return {
    url: `ws://127.0.0.1:${port.toString()}`,
    requests,
    close: () => new Promise<void>((resolve) => server.close(() => resolve())),
  };
};

/** A provider that only answers what `Lucid(...)` asks when it is built. */
const buildOnlyProvider = () => {
  const getProtocolParameters = vi.fn(async () => PROTOCOL_PARAMETERS_DEFAULT);
  return {
    provider: { getProtocolParameters } as unknown as Provider,
    getProtocolParameters,
  };
};

describe("da-bond on a Custom (local devnet) deployment (P25)", () => {
  // A whole second, about an hour before now.
  const genesisStartMs = Math.floor(Date.now() / 1_000) * 1_000 - 3_600_000;

  it("builds Lucid with the slot mapping the local Ogmios Shelley genesis gives", async () => {
    const ogmios = await fakeOgmios({ genesisStartMs });
    const { provider, getProtocolParameters } = buildOnlyProvider();
    try {
      const lucid = await daBondLucid({
        provider,
        network: "Custom",
        ogmiosUrl: ogmios.url,
      });
      expect(lucid.config().network).toBe("Custom");
      expect(lucid.config().slotConfig).toEqual({
        zeroTime: genesisStartMs,
        zeroSlot: 0,
        slotLength: 1_000,
      });
      expect(ogmios.requests).toEqual([
        "health",
        "queryNetwork/tip",
        "queryNetwork/genesisConfiguration",
      ]);
      expect(getProtocolParameters).toHaveBeenCalled();
    } finally {
      await ogmios.close();
    }
  });

  it("refuses a Custom deployment whose Shelley genesis query fails, with a named error and before Lucid is built", async () => {
    const ogmios = await fakeOgmios({ genesisStartMs, genesisFails: true });
    const { provider, getProtocolParameters } = buildOnlyProvider();
    try {
      const refused = daBondLucid({
        provider,
        network: "Custom",
        ogmiosUrl: ogmios.url,
      });
      await expect(refused).rejects.toBeInstanceOf(
        DaBondCustomSlotMappingError,
      );
      await expect(refused).rejects.toThrow(
        `Refusing the Custom deployment: the Shelley genesis query from the local Ogmios at ${ogmios.url} failed: HTTP 500`,
      );
      expect(getProtocolParameters).not.toHaveBeenCalled();
    } finally {
      await ogmios.close();
    }
  });

  const loadCustom = async (ogmiosUrl: string) => {
    const { provider, getProtocolParameters } = buildOnlyProvider();
    custom.active = true;
    custom.provider = provider;
    custom.reached = [];
    try {
      const loaded = await loadDaBondContext(
        {
          manifest: "custom-manifest.json",
          kupoUrl: "http://127.0.0.1:1442",
          ogmiosUrl,
        },
        {},
      ).then(
        () => undefined,
        (error: unknown) => error,
      );
      return { loaded, reached: custom.reached, getProtocolParameters };
    } finally {
      custom.active = false;
      custom.provider = undefined;
    }
  };

  it("loadDaBondContext admits a Custom manifest and builds its Lucid with the genesis-derived slot mapping", async () => {
    const ogmios = await fakeOgmios({ genesisStartMs });
    try {
      const { loaded, reached } = await loadCustom(ogmios.url);
      // It gets past the network parser and the manifest/Lucid network check
      // to the reference authentication, with the genesis mapping installed.
      expect(loaded).toEqual(new Error("stop after Lucid is built"));
      expect(reached).toHaveLength(1);
      const lucid = reached[0] as LucidEvolution;
      expect(lucid.config().network).toBe("Custom");
      expect(lucid.config().slotConfig).toEqual({
        zeroTime: genesisStartMs,
        zeroSlot: 0,
        slotLength: 1_000,
      });
      expect(ogmios.requests).toEqual([
        "health",
        "queryNetwork/tip",
        "queryNetwork/genesisConfiguration",
      ]);
    } finally {
      await ogmios.close();
    }
  });

  it("loadDaBondContext refuses a Custom manifest whose Shelley genesis query fails, before Lucid is built", async () => {
    const ogmios = await fakeOgmios({ genesisStartMs, genesisFails: true });
    try {
      const { loaded, reached, getProtocolParameters } = await loadCustom(
        ogmios.url,
      );
      expect(loaded).toBeInstanceOf(DaBondCustomSlotMappingError);
      expect((loaded as Error).message).toContain(
        `Refusing the Custom deployment: the Shelley genesis query from the local Ogmios at ${ogmios.url} failed: HTTP 500`,
      );
      expect(getProtocolParameters).not.toHaveBeenCalled();
      expect(reached).toEqual([]);
    } finally {
      await ogmios.close();
    }
  });

  it("keeps a public network's built-in slot mapping and never asks Ogmios for one", async () => {
    const ogmios = await fakeOgmios({ genesisStartMs });
    const { provider } = buildOnlyProvider();
    try {
      const lucid = await daBondLucid({
        provider,
        network: "Preprod",
        ogmiosUrl: ogmios.url,
      });
      expect(lucid.config().network).toBe("Preprod");
      expect(lucid.config().slotConfig).toEqual(SLOT_CONFIG_NETWORK.Preprod);
      expect(ogmios.requests).toEqual([]);
    } finally {
      await ogmios.close();
    }
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
