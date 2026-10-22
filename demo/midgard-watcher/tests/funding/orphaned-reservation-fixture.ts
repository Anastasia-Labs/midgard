import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.js";
import { createWatcherProverFundingAuthorityFactory } from "../../src/funding/prover-funding-authority.js";
import {
  deploymentIdentity,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";
import { TEST_JOURNAL_KEY } from "../support/watcher-journal-fixture.js";
import type { Fixture } from "./superseded-attempt-replacement.js";

/** The wallet holds enough for a second reservation beside the first. */
export const fundWallet = (fixture: Fixture) =>
  fixture.walletUtxos.push(
    ...[0, 1, 2, 3].map((outputIndex) => ({
      txHash: "98".repeat(32),
      outputIndex,
      address: walletAddress,
      assets: { lovelace: 2_000_000_000n },
    })),
  );

type FactoryInput = Parameters<
  typeof createWatcherProverFundingAuthorityFactory
>[0];

/** The production funding factory over the fixture's store, reading signed
 * attempts through `reads`. */
export const orphanReadingFactory = (
  fixture: Fixture,
  reads: Partial<
    Pick<FactoryInput, "observeSignedTransaction" | "now" | "alertSink">
  > = { observeSignedTransaction: fixture.observeSignedTransaction },
) => ({
  factory: createWatcherProverFundingAuthorityFactory({
    launchScope: installedLaunchScope(fixture.old.launchScope),
    journalRoot: fixture.journalRoot,
    journalAuthenticationKey: TEST_JOURNAL_KEY,
    deploymentIdentity,
    protocolParameters: fixture.protocolParameters,
    protocolParameterHistory: fixture.protocolParameterHistory,
    ...reads,
    store: fixture.store,
    workflowJournal: fixture.workflowJournal,
  }),
});
