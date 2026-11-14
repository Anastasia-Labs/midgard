/**
 * The host parts of the installed application a test stands in for: public
 * retained DA answered from the committed payload, the mutation-lease
 * service, and the clock the runner's pending loop polls.
 */
import { CML, type Emulator } from "@lucid-evolution/lucid";
import { vi } from "vitest";

import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSourceResult,
} from "../../src/transition-trace/fetch.js";

/** Retained DA answered from the committed block's payload envelope. */
export class FixtureRetainedDaSource extends DaLibp2pRetainedDaSource {
  readonly #payloads: ReadonlyMap<string, Buffer>;

  constructor(
    deploymentFingerprint: string,
    payloads: ReadonlyMap<string, Buffer>,
  ) {
    super({
      deploymentFingerprint,
      sourceId: "retained",
      peers: [{ peerId: "12D3KooWinstalledScriptDecode" }],
      transport: {
        request: async () => {
          throw new Error("the fixture source answers without a transport");
        },
      },
    });
    this.#payloads = payloads;
  }

  override async fetchPayloadByHeaderHash(
    headerHash: string,
  ): Promise<RetainedDaPayloadSourceResult> {
    const payload = this.#payloads.get(headerHash);
    if (payload === undefined) throw new Error("missing retained payload");
    return {
      ok: true,
      sourceId: "retained",
      sourcePeerId: "peer-1",
      attempts: [],
      payloadEnvelopeCbor: payload,
      provenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "retained/peer-1",
        grade: "security",
      },
    };
  }
}

/**
 * The emulator's clock as the application sees it: `Date.now` reads the
 * emulator, and every 1 s pending poll of the runner adds one block. Setting
 * `crashAfterPolls` makes the poll after that many throw once, standing in
 * for a process that dies between two journaled actions. `onPoll`, when
 * set, runs before the poll returns: the world acting while the runner
 * waits for a confirmation.
 */
export const installApplicationClock = (emulator: Emulator) => {
  const realSetTimeout = globalThis.setTimeout;
  const state = {
    polls: 0,
    crashAfterPolls: undefined as number | undefined,
    onPoll: undefined as (() => Promise<void>) | undefined,
    pollError: undefined as unknown,
  };
  const now = vi.spyOn(Date, "now").mockImplementation(() => emulator.now());
  const timers = vi.spyOn(globalThis, "setTimeout").mockImplementation(((
    callback: (...args: unknown[]) => void,
    ms?: number,
    ...args: unknown[]
  ) => {
    if (ms !== 1_000) return realSetTimeout(callback, ms, ...args);
    state.polls += 1;
    if (
      state.crashAfterPolls !== undefined &&
      state.polls > state.crashAfterPolls
    ) {
      state.crashAfterPolls = undefined;
      throw new Error("installed application process stopped");
    }
    emulator.awaitBlock();
    const onPoll = state.onPoll;
    if (onPoll === undefined) return realSetTimeout(callback, 0, ...args);
    return realSetTimeout(() => {
      void onPoll().then(
        () => callback(...args),
        (error: unknown) => {
          // The poll's promise has no reject path: surface the failure as
          // the stopped process it leaves.
          state.pollError = error;
          callback(...args);
        },
      );
    }, 0);
  }) as typeof setTimeout);
  return {
    state,
    restore: () => {
      timers.mockRestore();
      now.mockRestore();
    },
  };
};

const makeLease = (token: string) => ({
  token,
  source: "emulator",
  renew: async () => undefined,
  release: async () => undefined,
  fail: async (_reason: string) => undefined,
});

/** The separate mutation-lease service, durable across restarts. */
export const fixtureLeaseCoordinator = () => {
  const leases = new Map<string, ReturnType<typeof makeLease>>();
  return {
    acquire: async () => {
      const lease = makeLease(`script-decode-lease-${leases.size.toString()}`);
      leases.set(lease.token, lease);
      return lease;
    },
    resume: async ({ token, source }: { token: string; source: string }) => {
      const lease = leases.get(token);
      if (lease === undefined || lease.source !== source)
        throw new Error("Missing durable fixture lease");
      return lease;
    },
  };
};

/** What the ledger stand-in sees of a submitted transaction. */
export type InstalledSubmission = Readonly<{
  txHash: string;
  /** True when an output pays `address`. */
  paysTo: (address: string) => boolean;
  /** True when the transaction withdraws from a reward account. */
  withdraws: boolean;
}>;

/**
 * The ledger as the application submits to it: `decide` sees each
 * submission and may have the ledger reject it.
 */
export const interceptSubmissions = (
  emulator: Emulator,
  decide: (submission: InstalledSubmission) => "accept" | "reject",
) => {
  const submit = emulator.submitTx.bind(emulator);
  const spy = vi
    .spyOn(emulator, "submitTx")
    .mockImplementation(async (cbor: string) => {
      const body = CML.Transaction.from_cbor_hex(cbor).body();
      const outputs = body.outputs();
      const addresses = new Set<string>();
      for (let index = 0; index < outputs.len(); index += 1)
        addresses.add(outputs.get(index).address().to_bech32(undefined));
      const withdrawals = body.withdrawals();
      const verdict = decide({
        txHash: CML.hash_transaction(body).to_hex(),
        paysTo: (address) => addresses.has(address),
        withdraws: withdrawals !== undefined && withdrawals.len() > 0,
      });
      if (verdict === "reject")
        throw new Error("the ledger rejected the transaction");
      return await submit(cbor);
    });
  return { restore: () => spy.mockRestore() };
};
