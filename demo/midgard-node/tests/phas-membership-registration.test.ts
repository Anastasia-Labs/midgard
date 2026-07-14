import * as SDK from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";
import { vi } from "vitest";

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

const fakeLucid = (provider: unknown = {}): LucidEvolution =>
  ({
    config: () => ({
      network: "Preprod",
      provider,
    }),
  }) as unknown as LucidEvolution;

const mkKnownCredentialError = (scriptHash = phasIdentity.scriptHash) =>
  new TxSubmitError({
    message: `Failed to submit transaction: ${JSON.stringify({
      jsonrpc: "2.0",
      method: "submitTransaction",
      error: {
        code: 3145,
        message:
          "Trying to re-register some already known credentials. Stake credentials can only be registered once.",
        data: {
          from: "script",
          knownCredential: scriptHash,
        },
      },
    })}`,
    cause: "knownCredential",
    txHash: "00".repeat(32),
  });

const response = (body: unknown, init?: ResponseInit): Response =>
  new Response(typeof body === "string" ? body : JSON.stringify(body), init);

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
  it("treats Ogmios knownCredential stake registration failures as idempotent", () => {
    const scriptHash =
      "46df0027fc0af07197924dc07f1c27ac6b15eb2bd6efc7a73b0dbb4d";
    const providerError = {
      jsonrpc: "2.0",
      method: "submitTransaction",
      error: {
        code: 3145,
        message:
          "Trying to re-register some already known credentials. Stake credentials can only be registered once. This is true for both keys and scripts. The field 'data.knownCredential' points to an already known credential that's being re-registered by this transaction.",
        data: {
          from: "script",
          knownCredential: scriptHash,
        },
      },
    };
    const error = new TxSubmitError({
      message: `Failed to submit transaction: ${JSON.stringify(providerError)}`,
      cause: "knownCredential",
      txHash: "00".repeat(32),
    });

    expect(isPhasMembershipAlreadyRegisteredError(error, scriptHash)).toBe(
      true,
    );
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

  it("rejects a provider hash that is unrelated to the inspected body", async () => {
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

  it("keeps the submit-error race fallback after an unregistered preflight", async () => {
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
        submitRegistrationTx: () =>
          Effect.fail(mkKnownCredentialError(phasIdentity.scriptHash)),
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
    ).rejects.toMatchObject({
      message: "provider query failed",
    });
    expect(buildRegistrationTx).not.toHaveBeenCalled();
  });

  it("reads emulator registeredStake instead of delegation rewards", async () => {
    const registered = await Effect.runPromise(
      queryPhasMembershipRewardAccountRegisteredProgram(
        fakeLucid({
          chain: {
            [phasIdentity.rewardAddress]: {
              registeredStake: true,
              delegation: { poolId: null, rewards: 0n },
            },
          },
        }),
        phasIdentity.rewardAddress,
        phasIdentity.scriptHash,
      ),
    );
    const unregistered = await Effect.runPromise(
      queryPhasMembershipRewardAccountRegisteredProgram(
        fakeLucid({ chain: {} }),
        phasIdentity.rewardAddress,
        phasIdentity.scriptHash,
      ),
    );

    expect(registered).toBe("registered");
    expect(unregistered).toBe("unregistered");
  });

  it("queries Ogmios with the exact PHAS script hash and treats an empty summary as unknown", async () => {
    const fetchImpl = vi
      .fn()
      .mockResolvedValueOnce(
        response({
          jsonrpc: "2.0",
          id: null,
          result: [
            {
              from: "script",
              credential: phasIdentity.scriptHash,
              rewards: { ada: { lovelace: 0 } },
              deposit: { ada: { lovelace: 2_000_000 } },
            },
          ],
        }),
      )
      .mockResolvedValueOnce(
        response({
          jsonrpc: "2.0",
          id: null,
          result: [],
        }),
      );

    await expect(
      Effect.runPromise(
        queryPhasMembershipRewardAccountRegisteredProgram(
          fakeLucid({ ogmiosUrl: "http://ogmios.example" }),
          phasIdentity.rewardAddress,
          phasIdentity.scriptHash,
          fetchImpl,
        ),
      ),
    ).resolves.toBe("registered");
    await expect(
      Effect.runPromise(
        queryPhasMembershipRewardAccountRegisteredProgram(
          fakeLucid({ ogmiosUrl: "http://ogmios.example" }),
          phasIdentity.rewardAddress,
          phasIdentity.scriptHash,
          fetchImpl,
        ),
      ),
    ).resolves.toBe("unknown");

    for (const call of fetchImpl.mock.calls) {
      const init = call[1] as RequestInit;
      expect(JSON.parse(String(init.body))).toEqual({
        jsonrpc: "2.0",
        method: "queryLedgerState/rewardAccountSummaries",
        params: { scripts: [phasIdentity.scriptHash] },
        id: null,
      });
    }
  });

  it.each([
    { from: "script", credential: "ff".repeat(28) },
    { from: "verificationKey", credential: phasIdentity.scriptHash },
  ])("rejects a nonexact Ogmios summary: %o", async (summary) => {
    const fetchImpl = vi.fn().mockResolvedValue(
      response({
        jsonrpc: "2.0",
        id: null,
        result: [{ ...summary, rewards: { ada: { lovelace: 0 } } }],
      }),
    );

    await expect(
      Effect.runPromise(
        queryPhasMembershipRewardAccountRegisteredProgram(
          fakeLucid({ ogmiosUrl: "http://ogmios.example" }),
          phasIdentity.rewardAddress,
          phasIdentity.scriptHash,
          fetchImpl,
        ),
      ),
    ).rejects.toMatchObject({
      message: expect.stringContaining("credential_mismatch"),
    });
  });

  it("idempotently submits when provider evidence is unknown", async () => {
    const lucid = fakeLucid();
    const built = {
      tx: {} as TxSignBuilder,
      rewardAddress: phasIdentity.rewardAddress,
      scriptHash: phasIdentity.scriptHash,
    };
    const buildRegistrationTx = vi.fn(() => Effect.succeed(built));
    const submitRegistrationTx = vi.fn(() => Effect.succeed("bb".repeat(32)));

    const result = await Effect.runPromise(
      ensurePhasMembershipRewardAccountRegisteredProgram(lucid, {
        queryRegistration: () => Effect.succeed("unknown"),
        buildRegistrationTx,
        submitRegistrationTx,
        inspectRegistrationTx: () => capturedTransaction("bb".repeat(32)),
      }),
    );

    expect(result).toEqual({
      status: "registration_submitted",
      rewardAddress: phasIdentity.rewardAddress,
      scriptHash: phasIdentity.scriptHash,
      txHash: "bb".repeat(32),
      transactionBody: capturedTransaction("bb".repeat(32)).evidence,
    });
    expect(buildRegistrationTx).toHaveBeenCalledOnce();
    expect(submitRegistrationTx).toHaveBeenCalledWith(lucid, built);
  });

  it("tries configured local PHAS account lookup sources after retryable provider failures", async () => {
    const fetchImpl = vi
      .fn()
      .mockResolvedValueOnce(response({ error: "rate limit" }, { status: 429 }))
      .mockResolvedValueOnce(
        response({
          jsonrpc: "2.0",
          id: null,
          result: [
            {
              from: "script",
              credential: phasIdentity.scriptHash,
              rewards: { ada: { lovelace: 0 } },
            },
          ],
        }),
      );

    const result = await Effect.runPromise(
      queryPhasMembershipRewardAccountRegisteredProgram(
        fakeLucid({
          __midgardRewardAccountRegistrationSources: [
            {
              kind: "ogmios",
              source: "kupmios",
              url: "http://ogmios-a.example",
            },
            {
              kind: "ogmios",
              source: "kupmios",
              url: "http://ogmios-b.example",
            },
          ],
        }),
        phasIdentity.rewardAddress,
        phasIdentity.scriptHash,
        fetchImpl,
      ),
    );

    expect(result).toBe("registered");
    expect(fetchImpl).toHaveBeenCalledTimes(2);
    expect(fetchImpl.mock.calls[1]?.[0]).toBe("http://ogmios-b.example");
  });
});
