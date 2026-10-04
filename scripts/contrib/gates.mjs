import { existsSync } from "node:fs";
import { resolve } from "node:path";

import { filesUnder, packageByName } from "./files.mjs";
import { runTests } from "./tests.mjs";

// These are executable lanes over the production consumers. Adding a test
// title does not register evidence: receipts count assertions actually run.
export const GATES = {
  "deployment-fit": [
    [
      "midgard-node",
      [
        "tests/scratch-cg1-publication-fit.test.ts",
        "tests/published-workflow-deployment.test.ts",
        "tests/reference-publication-chain.test.ts",
      ],
    ],
    [
      "@al-ft/midgard-fault-proofs",
      ["tests/*publication-fit.test.ts", "tests/workflow.test.ts"],
    ],
  ],
  "recovery-scenarios": [
    [
      "@al-ft/midgard-fault-proofs",
      [
        "tests/workflow-terminal-recovery.test.ts",
        "tests/central-family-recovery.test.ts",
        "tests/distinct-asset-accumulation-recovery.test.ts",
        "tests/mint-item-terminal-recovery.test.ts",
        "tests/script-family-recovery.test.ts",
        "tests/submit-init-emulator-route-freedom-recovery.test.ts",
      ],
    ],
    [
      "midgard-node",
      [
        "tests/canonical-journal-recovery.test.ts",
        "tests/event-history-recovery-plans.test.ts",
        "tests/history-dependent-recovery-ordering.test.ts",
        "tests/commit-recovery-planner.test.ts",
      ],
    ],
    [
      "midgard-watcher",
      [
        "tests/funding/prover-funding-recovery.test.ts",
        "tests/runtime/history-recovery-durable.test.ts",
      ],
    ],
  ],
  "process-boundaries": [
    [
      "midgard-node",
      [
        "tests/commit-worker-failure-lease-classification.test.ts",
        "tests/validation-worker-pool.test.ts",
        "tests/l1-event-history-parent-lease-emulator.test.ts",
      ],
    ],
    ["@al-ft/midgard-fault-proofs", ["tests/workflow-runtime.test.ts"]],
  ],
  lifecycle: [
    [
      "midgard-node",
      [
        "tests/provider-retry.test.ts",
        "tests/startup-protocol-status-retry.test.ts",
        "tests/history-source-owner-streaming.test.ts",
      ],
    ],
    [
      "midgard-node-tools",
      [
        "tests/process-ownership.test.ts",
        "tests/e2e-service-supervisor.test.ts",
        "tests/phase4-process-output.test.ts",
      ],
    ],
    ["@al-ft/midgard-fault-proofs", ["tests/workflow-kupmios-source.test.ts"]],
  ],
  "runtime-progress": [
    [
      "midgard-node",
      [
        "tests/readiness.test.ts",
        "tests/readiness-history-frontier-route.test.ts",
        "tests/pipeline-status-route.test.ts",
      ],
    ],
    [
      "midgard-watcher",
      [
        "tests/runtime/startup-progress.test.ts",
        "tests/runtime/chain-coordinator-progress.test.ts",
        "tests/fault-proofs/fault-proof-objective-progress.test.ts",
      ],
    ],
  ],
  "policy-matrix": [
    [
      "midgard-node",
      [
        "tests/startup-policy.test.ts",
        "tests/retention-policy.test.ts",
        "tests/submit-timing.test.ts",
      ],
    ],
    [
      "@al-ft/midgard-fault-proofs",
      [
        "tests/runtime-funding-policy.test.ts",
        "tests/workflow-runtime.test.ts",
      ],
    ],
    [
      "midgard-watcher",
      [
        "tests/funding/workflow-funding-profile-overlay.test.ts",
        "tests/storage/durable-runtime.test.ts",
      ],
    ],
  ],
  "input-envelopes": [
    [
      "@al-ft/midgard-fault-proofs",
      [
        "tests/*envelope.test.ts",
        "tests/*maximum*.test.ts",
        "tests/submit-init-emulator-committed-field-shape-adversarial.test.ts",
        "tests/retained-classification-polarities.test.ts",
        "tests/validation-dispute-origin-classification.test.ts",
      ],
    ],
    [
      "da-committee-node",
      ["tests/cardano-capability-retained-da-corpus.test.ts"],
    ],
  ],
  "database-isolation": [
    [
      "midgard-node",
      [
        "tests/database.test.ts",
        "tests/migration-runner.test.ts",
        "tests/migration-locking.test.ts",
        "tests/tx-admissions-claim-load.test.ts",
      ],
    ],
  ],
};

export const gatePlan = (root, name) => {
  if (!GATES[name])
    throw new Error(
      `unknown gate ${name}; choose ${Object.keys(GATES).join(", ")}`,
    );
  return GATES[name].map(([name, patterns]) => {
    const pkg = packageByName(root, name);
    const all = filesUnder(resolve(root, pkg.directory)).map((file) =>
      file.slice(resolve(root, pkg.directory).length + 1),
    );
    const selected = patterns.flatMap((pattern) => {
      if (!pattern.includes("*")) {
        if (!existsSync(resolve(root, pkg.directory, pattern)))
          throw new Error(
            `gate ${name} has a missing owner: ${pattern}; repair its registry`,
          );
        return [pattern];
      }
      const regex = new RegExp(
        `^${pattern
          .split("*")
          .map((part) => part.replace(/[.*+?^${}()|[\]\\]/gu, "\\$&"))
          .join("[^/]*")}$`,
        "u",
      );
      const matching = all.filter((file) => regex.test(file));
      if (!matching.length)
        throw new Error(`gate pattern collected no files: ${pattern}`);
      return matching;
    });
    return { package: pkg.name, files: [...new Set(selected)].sort() };
  });
};

export const runGate = async (root, name, options) => {
  const plan = gatePlan(root, name);
  const receipts = [];
  for (const lane of plan)
    receipts.push(
      await runTests(root, lane.package, { ...options, files: lane.files }),
    );
  return {
    gate: name,
    receipts: receipts.map((receipt) => ({
      path: receipt.path,
      status: receipt.status,
      counts: receipt.counts,
    })),
    exitCode: receipts.some((receipt) => receipt.exitCode === 1)
      ? 1
      : receipts.some((receipt) => receipt.exitCode === 3)
        ? 3
        : 0,
  };
};
