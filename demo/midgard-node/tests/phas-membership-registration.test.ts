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
  ensurePhasMembershipRewardAccountRegisteredProgram,
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
        queryRegistration: () => Effect.succeed(true),
        buildRegistrationTx,
        submitRegistrationTx,
      }),
    );

    expect(result).toEqual({
      status: "already_registered",
      rewardAddress: phasIdentity.rewardAddress,
      scriptHash: phasIdentity.scriptHash,
      txHash: null,
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
        queryRegistration: () => Effect.succeed(false),
        buildRegistrationTx,
        submitRegistrationTx,
      }),
    );

    expect(result).toEqual({
      status: "registration_submitted",
      rewardAddress: phasIdentity.rewardAddress,
      scriptHash: phasIdentity.scriptHash,
      txHash: "aa".repeat(32),
    });
    expect(buildRegistrationTx).toHaveBeenCalledOnce();
    expect(submitRegistrationTx).toHaveBeenCalledWith(lucid, built);
  });

  it("keeps the submit-error race fallback after an unregistered preflight", async () => {
    const built = {
      tx: {} as TxSignBuilder,
      rewardAddress: phasIdentity.rewardAddress,
      scriptHash: phasIdentity.scriptHash,
    };

    const result = await Effect.runPromise(
      ensurePhasMembershipRewardAccountRegisteredProgram(fakeLucid(), {
        queryRegistration: () => Effect.succeed(false),
        buildRegistrationTx: () => Effect.succeed(built),
        submitRegistrationTx: () =>
          Effect.fail(mkKnownCredentialError(phasIdentity.scriptHash)),
      }),
    );

    expect(result).toEqual({
      status: "already_registered",
      rewardAddress: phasIdentity.rewardAddress,
      scriptHash: phasIdentity.scriptHash,
      txHash: null,
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
      ),
    );
    const unregistered = await Effect.runPromise(
      queryPhasMembershipRewardAccountRegisteredProgram(
        fakeLucid({ chain: {} }),
        phasIdentity.rewardAddress,
      ),
    );

    expect(registered).toBe(true);
    expect(unregistered).toBe(false);
  });

  it("queries Ogmios reward account summaries for Kupmios providers", async () => {
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
          fetchImpl,
        ),
      ),
    ).resolves.toBe(true);
    await expect(
      Effect.runPromise(
        queryPhasMembershipRewardAccountRegisteredProgram(
          fakeLucid({ ogmiosUrl: "http://ogmios.example" }),
          phasIdentity.rewardAddress,
          fetchImpl,
        ),
      ),
    ).resolves.toBe(false);
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
        fetchImpl,
      ),
    );

    expect(result).toBe(true);
    expect(fetchImpl).toHaveBeenCalledTimes(2);
    expect(fetchImpl.mock.calls[1]?.[0]).toBe("http://ogmios-b.example");
  });
});
