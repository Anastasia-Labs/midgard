/**
 * Worker-thread entry for tests/settlement-worker-heap.test.ts, bundled from
 * source and run under the settlement worker's heap limit. It loads the
 * settlement worker's whole module graph, as the real worker does, then
 * resolves the settlement's reference scripts from a Kupo endpoint.
 */
import "../../src/services/settlement.js";

import { getHeapStatistics } from "node:v8";
import { parentPort, workerData } from "node:worker_threads";

import {
  Kupmios,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
  type Script,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { fetchReferenceScriptUtxosProgram } from "../../src/transactions/reference-scripts.js";

export type SettlementHeapProbeInput = {
  readonly kupoUrl: string;
  readonly address: string;
  readonly policyId: string;
  readonly targets: readonly { name: string; script: Script }[];
};

export type SettlementHeapProbeResult = {
  readonly resolved: readonly { name: string; outRef: string }[];
  readonly usedHeapMb: number;
};

if (parentPort !== null) {
  const port = parentPort;
  const input = workerData as SettlementHeapProbeInput;
  const lucid = await Lucid(
    new Kupmios(input.kupoUrl, "http://127.0.0.1:9"),
    "Preprod",
    { presetProtocolParameters: PROTOCOL_PARAMETERS_DEFAULT },
  );
  const resolved = await Effect.runPromise(
    fetchReferenceScriptUtxosProgram(lucid, input.address, input.targets, {
      policyId: input.policyId,
    }),
  );
  port.postMessage({
    resolved: resolved.map(({ name, utxo }) => ({
      name,
      outRef: `${utxo.txHash}#${utxo.outputIndex}`,
    })),
    usedHeapMb: getHeapStatistics().used_heap_size / 2 ** 20,
  } satisfies SettlementHeapProbeResult);
}
