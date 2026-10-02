import { SqlClient } from "@effect/sql";
import { CML, OgmiosJsonRpcError } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, vi } from "vitest";

import {
  liveRebroadcastDeps,
  rebroadcastOnce,
  type RebroadcastState,
} from "../../src/fibers/signed-intent-rebroadcast.js";
import { COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../../src/workers/utils/commit-end-time.js";
import {
  advanceEmulatorToDueWork,
  alignCommitSchedulerBeforeTestWorker,
  fetchLatestCommittedBlock,
  runCommitWorker,
} from "../deposit-flow-emulator-shared.js";
import {
  read,
  readJournal,
  synchronizeBounded,
} from "./correction-rewind-scenario.js";
import {
  attestBaseInPlace,
  type Lifecycle,
  signedInvalidBefore,
} from "./signed-intent-early-release.js";
import { readEmulatorQueue, signedTtl } from "./signed-intent-replacement.js";

/**
 * A signed commit E on D that the node's own submit path hands to a provider
 * that refuses it with Ogmios error 3117 (unknown inputs), after the commit
 * worker persisted its signed intent and checked the live tail:
 * - `foreign_spend`: between that check and the provider's answer, D's DA
 *   attestation (another transaction) spends D's output, and the provider
 *   refuses E naming the inputs it no longer holds;
 * - `provider_lag`: a lagging provider refuses E naming D's output, which is
 *   unspent, and E still can land.
 */

const inputsOf = (signedCbor: string) => {
  const tx = CML.Transaction.from_cbor_hex(signedCbor);
  const body = tx.body();
  const inputs = body.inputs();
  const outRefs: string[] = [];
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    outRefs.push(`${input.transaction_id().to_hex()}#${input.index()}`);
    input.free();
  }
  inputs.free();
  body.free();
  tx.free();
  return outRefs;
};

/** The provider's refusal of a transaction whose inputs it does not know. */
export const unknownInputsRefusal = (outRefs: readonly string[]) =>
  new OgmiosJsonRpcError({
    code: 3117,
    message:
      "The transaction contains unknown UTxO references as inputs. This can happen if the inputs you're trying to spend have already been spent, or if you've simply referred to non-existing UTxO altogether. The field 'data.unknownOutputReferences' indicates all unknown inputs.",
    data: {
      unknownOutputReferences: outRefs.map((outRef) => {
        const [id, index] = outRef.split("#");
        return { transaction: { id: id! }, index: Number(index) };
      }),
    },
    method: "submitTransaction",
    id: null,
  });

const headerOfIntent = (txHash: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{ header_hash: Buffer }>`SELECT header_hash
        FROM pending_block_finalizations
        WHERE intended_tx_hash = ${Buffer.from(txHash, "hex")}`;
      expect(rows).toHaveLength(1);
      return rows[0]!.header_hash.toString("hex");
    }),
  );

export const commitRefusedAsUnknownInputs = async (
  h: Lifecycle,
  base: string,
  inclusion: number,
  mode: "foreign_spend" | "provider_lag",
) => {
  const { fixture, lucidService, globals, production } = h;
  const { emulator } = fixture;
  const tail = (await readEmulatorQueue(h)).find(
    (node) => node.headerHash === base,
  );
  expect(tail).toBeDefined();
  const submit = emulator.submitTx;
  let refused:
    | {
        signed: string;
        unknown: readonly string[];
        attested?: Awaited<ReturnType<typeof attestBaseInPlace>>;
      }
    | undefined;
  emulator.submitTx = async (signedCbor) => {
    if (refused !== undefined || !inputsOf(signedCbor).includes(tail!.outRef))
      return submit(signedCbor);
    if (mode === "provider_lag") {
      refused = { signed: signedCbor, unknown: [tail!.outRef] };
      throw unknownInputsRefusal(refused.unknown);
    }
    refused = { signed: signedCbor, unknown: [] };
    const attested = await attestBaseInPlace(h, base);
    // The ledger refuses E: an input is gone.
    const ledger = await submit(signedCbor).then(
      () => "accepted",
      (cause: unknown) =>
        String(cause instanceof Error ? cause.message : cause),
    );
    expect(ledger).toMatch(/already spent/);
    const unknown: string[] = [];
    for (const outRef of inputsOf(signedCbor)) {
      const [txHash, outputIndex] = outRef.split("#");
      const held = await fixture.operatorLucid.utxosByOutRef([
        { txHash: txHash!, outputIndex: Number(outputIndex) },
      ]);
      if (held.length === 0) unknown.push(outRef);
    }
    expect(unknown).toContain(tail!.outRef);
    refused = { signed: signedCbor, unknown, attested };
    throw unknownInputsRefusal(unknown);
  };
  let output: Awaited<ReturnType<typeof runCommitWorker>>;
  try {
    await h.deployment.chain.awaitLedgerTime(inclusion + 1000);
    vi.setSystemTime(emulator.now());
    await synchronizeBounded(h);
    await alignCommitSchedulerBeforeTestWorker({
      fixture,
      lucidService,
      targetEndTimeMs: Date.now() + COMMIT_MINIMUM_FUTURE_BUFFER_MS,
    });
    const latestBlock = await fetchLatestCommittedBlock(
      fixture.operatorLucid,
      fixture.contracts,
    );
    for (let attempt = 1; ; attempt += 1) {
      await h.synchronize();
      output = await runCommitWorker(
        fixture.contracts,
        lucidService,
        latestBlock,
        production.nodeConfig,
        fixture.runtimeOverrides!.deploymentIdentity,
        { ...production, globals },
      );
      if (output?.type !== "RegisteredDueWorkOutput" || attempt === 4) break;
      await advanceEmulatorToDueWork(fixture, output.dueWork);
    }
  } finally {
    emulator.submitTx = submit;
  }
  // The refused submit leaves a wallet view pinned to its predicted change.
  lucidService.api.clearUTxOOverride();
  fixture.operatorLucid.clearUTxOOverride();
  expect(refused).toBeDefined();
  const signed = Buffer.from(refused!.signed, "hex");
  const tx = CML.Transaction.from_cbor_bytes(signed);
  const body = tx.body();
  const txHash = CML.hash_transaction(body).to_hex();
  body.free();
  tx.free();
  const header = await headerOfIntent(txHash);
  return {
    output,
    refused: refused!,
    header,
    journal: await readJournal(header),
    signed,
    txHash,
    ttl: signedTtl(signed),
    invalidBefore: signedInvalidBefore(signed),
    baseOutRef: tail!.outRef,
  };
};

/** The rebroadcast fiber's pass over the node's live dependencies, on a
 * clock the test moves, recording every body it submits. */
export const liveRebroadcaster = async (h: Lifecycle) => {
  const deps = await h.runWithoutSynchronizing(liveRebroadcastDeps);
  const submitted: string[] = [];
  const state: RebroadcastState = new Map();
  const clock = { now: 0 };
  const once = () =>
    Effect.runPromise(
      rebroadcastOnce(
        {
          ...deps,
          submit: (cbor) => {
            submitted.push(cbor);
            return deps.submit(cbor);
          },
          nowMs: () => clock.now,
        },
        state,
      ),
    );
  return { once, submitted, clock };
};
