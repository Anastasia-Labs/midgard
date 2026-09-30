import { Effect } from "effect";

import { projectEventHistoryBlock } from "../src/l1-event-history-projection.js";
import {
  decodeBoundEventHistoryLedgerSnapshot,
  type EventHistorySourceBinding,
  HISTORY_GENESIS_DIGEST_ALGORITHM,
} from "../src/l1-event-history-source.js";
import {
  binding,
  fixture,
  nodeOutput,
  pair,
  rootNode,
  slotToUnixTime,
  ttl,
} from "./l1-event-history-transition.fixture.js";

export const projectionFixture = async (external = false) => {
  const fixtureValue = fixture("deposit", external);
  // A structural source receipt for this pure projection test. Exact socket
  // authentication and manifest admission are independently tested upstream.
  const source: EventHistorySourceBinding = {
    ...binding,
    digest: "f0".repeat(32),
    manifestId: "f1".repeat(32),
    genesisSha256: "f3".repeat(32),
    genesisAlgorithm: HISTORY_GENESIS_DIGEST_ALGORITHM,
  };
  const ledger = {
    point: { slot: 1, id: "44".repeat(32) },
    addresses: [
      binding.hubAddress,
      ...Object.values(binding.deployments).flatMap((deployment) => [
        deployment.address,
        deployment.retentionAddress,
      ]),
    ],
    outputs: [
      ...fixtureValue.params.currentNodes,
      nodeOutput("withdrawal", rootNode(), 0, "cc".repeat(32)),
      ...fixtureValue.references,
    ],
  };
  const previous = await Effect.runPromise(
    decodeBoundEventHistoryLedgerSnapshot(ledger, source),
  );
  return {
    fixtureValue,
    params: {
      previous,
      binding: source,
      histories: pair,
      block: {
        point: { slot: ttl, id: "55".repeat(32), height: 2 },
        parent: ledger.point.id,
        transactions: [fixtureValue.params.transaction],
      },
      resolveReference: () => undefined,
      slotToUnixTime,
    } satisfies Parameters<typeof projectEventHistoryBlock>[0],
  };
};
