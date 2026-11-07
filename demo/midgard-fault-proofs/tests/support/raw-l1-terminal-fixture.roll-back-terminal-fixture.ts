import {
  ConfirmedState,
  encodeLinkedListNodeView,
  makeGenesisConfirmedState,
  STATE_QUEUE_ROOT_ASSET_NAME,
} from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";

import { type FraudProofRawL1Snapshot } from "../../src/workflow/index.js";
import { fixture } from "./raw-l1-terminal-fixture.fixture.js";
import { hash32, output, raw } from "./raw-l1-terminal-fixture.output.js";

/** Canonical fixture rewind retaining the authenticated header/proof mints. */
export const rollBackTerminalFixture = ({
  snapshot,
  definition,
}: Awaited<ReturnType<typeof fixture>>): FraudProofRawL1Snapshot => {
  const removal = snapshot.transactions[0]!;
  const [target, bond] = removal.resolvedInputs;
  if (target === undefined || bond === undefined)
    throw new Error("fixture requires its exact removal inputs");
  const root = raw(
    `${hash32("63")}#0`,
    output({
      address: definition.stateQueue.address,
      assets: {
        lovelace: 3_000_000n,
        [toUnit(definition.stateQueue.policyId, STATE_QUEUE_ROOT_ASSET_NAME)]:
          1n,
      },
      datum: encodeLinkedListNodeView({
        key: "Empty",
        next: { Key: { key: definition.headerHash } },
        data: Data.from(Data.to(makeGenesisConfirmedState(0n), ConfirmedState)),
      }),
    }),
  );
  return {
    ...snapshot,
    transactions: snapshot.transactions.filter(
      ({ txHash }) => txHash !== removal.txHash,
    ),
    history: snapshot.history.map((entry) => ({
      ...entry,
      transactionHashes: entry.transactionHashes.filter(
        (txHash) => txHash !== removal.txHash,
      ),
    })),
    scopes: snapshot.scopes.map((scope) =>
      scope.role === "state_queue"
        ? { ...scope, utxos: [root, target] }
        : scope.role === "active_operator_directory"
          ? { ...scope, utxos: [bond] }
          : scope,
    ),
  };
};
