/**
 * The watcher's pre-mint funding recovery authorization and the durable
 * layout it reads, for installed-application tests in other packages that
 * mint prover funding without the manifest-derived factory: each mint passes
 * the same gate the factory runs first.
 */
import type { HeaderFaultDecision } from "@al-ft/midgard-fault-proofs";

import { openWatcherFaultDecisionJournal } from "../../src/fault-proofs/fault-decision-journal.js";
import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.js";
import { TEST_JOURNAL_KEY } from "./watcher-journal-fixture.js";

export { authorizeWatcherProverFundingRecovery } from "../../src/funding/prover-funding-recovery.js";
export { TEST_JOURNAL_KEY };

/** Records an admitted fault decision in the watcher's decision journal under
 * `journalRoot`, where recovery looks up a workflow's original decision. */
export const recordWatcherFaultDecision = async (
  journalRoot: string,
  decision: HeaderFaultDecision,
) =>
  await (
    await openWatcherFaultDecisionJournal({
      directory: journalRoot,
      deploymentFingerprint: decision.deploymentFingerprint,
      launchScope: installedLaunchScope(decision.launchScope),
      authenticationKey: TEST_JOURNAL_KEY,
    })
  ).appendLiveDecision(decision);
