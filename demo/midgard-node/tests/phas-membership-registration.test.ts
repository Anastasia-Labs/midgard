import * as SDK from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  OgmiosJsonRpcError,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { loadPhasMembershipWithdrawalScript } from "@/phas-membership.js";
import {
  type CapturedPhasMembershipRegistrationTransaction,
  ensurePhasMembershipRewardAccountRegisteredProgram,
  inspectPhasMembershipRegistrationTransaction,
  isPhasMembershipAlreadyRegisteredError,
  queryPhasMembershipRewardAccountRegisteredProgram,
} from "@/transactions/phas-membership-registration.js";
import { TxSubmitError } from "@/transactions/utils.js";

const phasIdentity = SDK.phasMembershipIdentity(
  "Preprod",
  loadPhasMembershipWithdrawalScript(),
);

const fakeLucid = (
  rewardAccountAt: LucidEvolution["rewardAccountAt"] = async () => ({
    registered: false,
    poolId: null,
    rewards: 0n,
  }),
): LucidEvolution =>
  ({
    config: () => ({ network: "Preprod", provider: {} }),
    rewardAccountAt,
  }) as unknown as LucidEvolution;

const mkKnownCredentialError = (scriptHash = phasIdentity.scriptHash) =>
  new TxSubmitError({
    message: "Failed to submit transaction",
    cause: new OgmiosJsonRpcError({
      code: 3145,
      message:
        "Trying to re-register some already known credentials. Stake credentials can only be registered once.",
      data: { from: "script", knownCredential: scriptHash },
      method: "submitTransaction",
      id: null,
    }),
    txHash: "00".repeat(32),
  });

const capturedTransaction = (
  txHash: string,
): CapturedPhasMembershipRegistrationTransaction => ({
  unsignedTransactionCborHex: "84a0a0f5f6",
  evidence: {
    schemaVersion: "midgard-phas-registration-transaction-body-v1",
    txHash,
    cborSha256: "cc".repeat(32),
    cborSizeBytes: 5,
    certificate: {
      kind: "stake_registration",
      index: 0,
      count: 1,
      credentialType: "script",
      scriptHash: phasIdentity.scriptHash,
    },
  },
});

describe("PHAS membership reward registration", () => {
  it("uses typed Ogmios knownCredential data for the idempotent race", () => {
    expect(
      isPhasMembershipAlreadyRegisteredError(
        mkKnownCredentialError(),
        phasIdentity.scriptHash,
      ),
    ).toBe(true);
  });

  it("rejects formatted text and mismatched credentials as race evidence", () => {
    const formattedOnly = new TxSubmitError({
      message: `knownCredential=${phasIdentity.scriptHash}`,
      cause: "knownCredential",
      txHash: "00".repeat(32),
    });

    expect(
      isPhasMembershipAlreadyRegisteredError(
        formattedOnly,
        phasIdentity.scriptHash,
      ),
    ).toBe(false);
    expect(
      isPhasMembershipAlreadyRegisteredError(
        mkKnownCredentialError("ff".repeat(28)),
        phasIdentity.scriptHash,
      ),
    ).toBe(false);
  });

  it("uses Lucid's provider-neutral reward-account status", async () => {
    const registeredQuery = vi.fn(async () => ({
      registered: true,
      poolId: null,
      rewards: 0n,
    }));
    const unregisteredQuery = vi.fn(async () => ({
      registered: false,
      poolId: null,
      rewards: 0n,
    }));

    await expect(
      Effect.runPromise(
        queryPhasMembershipRewardAccountRegisteredProgram(
          fakeLucid(registeredQuery),
          phasIdentity.rewardAddress,
        ),
      ),
    ).resolves.toBe("registered");
    await expect(
      Effect.runPromise(
        queryPhasMembershipRewardAccountRegisteredProgram(
          fakeLucid(unregisteredQuery),
          phasIdentity.rewardAddress,
        ),
      ),
    ).resolves.toBe("unregistered");
    expect(registeredQuery).toHaveBeenCalledWith(phasIdentity.rewardAddress);
    expect(unregisteredQuery).toHaveBeenCalledWith(phasIdentity.rewardAddress);
  });

  it("fails closed when Lucid cannot query reward-account status", async () => {
    await expect(
      Effect.runPromise(
        queryPhasMembershipRewardAccountRegisteredProgram(
          fakeLucid(async () => {
            throw new Error("provider unavailable");
          }),
          phasIdentity.rewardAddress,
        ),
      ),
    ).rejects.toMatchObject({
      message: expect.stringContaining(
        "Failed to query PHAS reward-account registration status",
      ),
    });
  });

  it("returns already_registered before building when the reward account is registered", async () => {
    const buildRegistrationTx = vi.fn(() =>
      Effect.fail(
        new SDK.LucidError({
          message: "builder should not be called",
          cause: null,
        }),
      ),
    );
    const submitRegistrationTx = vi.fn(() =>
      Effect.fail(
        new TxSubmitError({
          message: "submitter should not be called",
          cause: null,
          txHash: "00".repeat(32),
        }),
      ),
    );

    const result = await Effect.runPromise(
      ensurePhasMembershipRewardAccountRegisteredProgram(fakeLucid(), {
        queryRegistration: () => Effect.succeed("registered"),
        buildRegistrationTx,
        submitRegistrationTx,
      }),
    );

    expect(result).toEqual({
      status: "already_registered",
      rewardAddress: phasIdentity.rewardAddress,
      scriptHash: phasIdentity.scriptHash,
      txHash: null,
      transactionBody: null,
    });
    expect(buildRegistrationTx).not.toHaveBeenCalled();
    expect(submitRegistrationTx).not.toHaveBeenCalled();
  });

  it("builds and submits only when the preflight reports unregistered", async () => {
    const lucid = fakeLucid();
    const built = {
      tx: {} as TxSignBuilder,
      rewardAddress: phasIdentity.rewardAddress,
      scriptHash: phasIdentity.scriptHash,
    };
    const buildRegistrationTx = vi.fn(() => Effect.succeed(built));
    const submitRegistrationTx = vi.fn(() => Effect.succeed("aa".repeat(32)));

    const result = await Effect.runPromise(
      ensurePhasMembershipRewardAccountRegisteredProgram(lucid, {
        queryRegistration: () => Effect.succeed("unregistered"),
        buildRegistrationTx,
        submitRegistrationTx,
        inspectRegistrationTx: () => capturedTransaction("aa".repeat(32)),
      }),
    );

    expect(result).toEqual({
      status: "registration_submitted",
      rewardAddress: phasIdentity.rewardAddress,
      scriptHash: phasIdentity.scriptHash,
      txHash: "aa".repeat(32),
      transactionBody: capturedTransaction("aa".repeat(32)).evidence,
    });
    expect(buildRegistrationTx).toHaveBeenCalledOnce();
    expect(submitRegistrationTx).toHaveBeenCalledWith(lucid, built);
  });

  it("keeps the typed submit-error race fallback after an unregistered preflight", async () => {
    const built = {
      tx: {} as TxSignBuilder,
      rewardAddress: phasIdentity.rewardAddress,
      scriptHash: phasIdentity.scriptHash,
    };

    const result = await Effect.runPromise(
      ensurePhasMembershipRewardAccountRegisteredProgram(fakeLucid(), {
        queryRegistration: () => Effect.succeed("unregistered"),
        buildRegistrationTx: () => Effect.succeed(built),
        inspectRegistrationTx: () => capturedTransaction("00".repeat(32)),
        submitRegistrationTx: () => Effect.fail(mkKnownCredentialError()),
      }),
    );

    expect(result).toEqual({
      status: "already_registered",
      rewardAddress: phasIdentity.rewardAddress,
      scriptHash: phasIdentity.scriptHash,
      txHash: null,
      transactionBody: null,
    });
  });

  it("rejects a built registration for any noncanonical PHAS identity", () => {
    expect(() =>
      inspectPhasMembershipRegistrationTransaction(
        {
          tx: {} as TxSignBuilder,
          rewardAddress: `${phasIdentity.rewardAddress}x`,
          scriptHash: phasIdentity.scriptHash,
        },
        phasIdentity,
      ),
    ).toThrow("Built PHAS registration identity mismatch");
  });

  it("rejects a provider hash unrelated to the inspected body", async () => {
    const built = {
      tx: {} as TxSignBuilder,
      rewardAddress: phasIdentity.rewardAddress,
      scriptHash: phasIdentity.scriptHash,
    };

    await expect(
      Effect.runPromise(
        ensurePhasMembershipRewardAccountRegisteredProgram(fakeLucid(), {
          queryRegistration: () => Effect.succeed("unregistered"),
          buildRegistrationTx: () => Effect.succeed(built),
          inspectRegistrationTx: () => capturedTransaction("aa".repeat(32)),
          submitRegistrationTx: () => Effect.succeed("bb".repeat(32)),
        }),
      ),
    ).rejects.toMatchObject({
      message: expect.stringContaining(
        "does not match the verified unsigned transaction body",
      ),
    });
  });

  it("fails provider query errors before building a registration transaction", async () => {
    const buildRegistrationTx = vi.fn(() =>
      Effect.succeed({
        tx: {} as TxSignBuilder,
        rewardAddress: phasIdentity.rewardAddress,
        scriptHash: phasIdentity.scriptHash,
      }),
    );

    await expect(
      Effect.runPromise(
        ensurePhasMembershipRewardAccountRegisteredProgram(fakeLucid(), {
          queryRegistration: () =>
            Effect.fail(
              new SDK.LucidError({
                message: "provider query failed",
                cause: null,
              }),
            ),
          buildRegistrationTx,
        }),
      ),
    ).rejects.toMatchObject({ message: "provider query failed" });
    expect(buildRegistrationTx).not.toHaveBeenCalled();
  });
});
