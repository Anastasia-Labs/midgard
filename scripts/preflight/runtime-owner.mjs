// Scheduling only: a hosted obligation is pending, never a local pass.
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { loadYaml } from "../ci/lint-workflows.mjs";
import { matchesAny } from "./derive.mjs";

export const RUNTIME_CHECKS = [
  "demo-build",
  "demo-typecheck",
  "demo-test",
  "demo-test-db",
  "tx-preparation:sdk",
];
export const RUNTIME_WORKFLOW = ".github/workflows/midgard-node-ci.yml";

const sdkLaneMatches = (root) => {
  try {
    return (
      JSON.parse(readFileSync(resolve(root, "demo/package.json"), "utf8"))
        .scripts?.["test:tx-prep:sdk"] ===
      "pnpm --filter @al-ft/lucid-midgard test && pnpm --filter @al-ft/midgard-sdk test"
    );
  } catch {
    return false;
  }
};

// Unknown workflow routing or an ineffective summary gate retains local work.
// The coverage regression tests bind these obligations to actual package jobs.
export const readRuntimeWorkflow = (root, { yaml = loadYaml(root) } = {}) => {
  try {
    if (!yaml) return undefined;
    const workflow = yaml.parse(
      readFileSync(resolve(root, RUNTIME_WORKFLOW), "utf8"),
    );
    const trigger = workflow.on?.pull_request;
    if (!trigger || Object.keys(trigger).some((key) => key !== "paths"))
      return undefined;
    if (
      !Array.isArray(trigger.paths) ||
      trigger.paths.some(
        (path) => typeof path !== "string" || path.startsWith("!"),
      )
    )
      return undefined;
    const { gate, ...jobs } = workflow.jobs ?? {};
    if (
      gate?.if !== "${{ !cancelled() }}" ||
      gate["continue-on-error"] ||
      !Array.isArray(gate.needs)
    )
      return undefined;
    if (
      Object.entries(jobs).some(
        ([id, job]) =>
          !gate.needs.includes(id) ||
          (job.if !== undefined && job.if !== "${{ !cancelled() }}") ||
          job["continue-on-error"],
      )
    )
      return undefined;
    if (
      !gate.steps?.some(
        (step) =>
          step.run ===
            "node scripts/ci/check-needs-results.mjs --allow success" &&
          step.env?.NEEDS === "${{ toJSON(needs) }}" &&
          step.if === undefined &&
          !step["continue-on-error"],
      )
    )
      return undefined;
    return {
      paths: trigger.paths,
      workflow: RUNTIME_WORKFLOW,
      gate: "Node CI gate",
      sdkLane: sdkLaneMatches(root),
    };
  } catch {
    return undefined;
  }
};

export const ciOwnsRuntime = (registry, changed, owner) =>
  owner === "ci" &&
  registry.runtimeWorkflow !== undefined &&
  changed.some((path) => matchesAny(path, registry.runtimeWorkflow.paths));
