import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterAll, describe, expect, it } from "vitest";

import {
  JOURNEY_DEPLOYMENT_ENV,
  selectJourneyDeploymentSource,
} from "./deployment-source.js";
import { loadJourneyContext } from "./live-context.js";

const directories: string[] = [];
afterAll(() =>
  Promise.all(
    directories.map((path) => rm(path, { recursive: true, force: true })),
  ),
);
const emptyDirectory = async () => {
  const directory = await mkdtemp(join(tmpdir(), "journey-source-"));
  directories.push(directory);
  return directory;
};

const failureOf = async (action: () => Promise<unknown>) => {
  try {
    await action();
  } catch (cause) {
    return String(cause);
  }
  throw new Error("Expected a failure");
};

const stackEnv = {
  [JOURNEY_DEPLOYMENT_ENV.mode]: "stack",
  [JOURNEY_DEPLOYMENT_ENV.stackConfig]: "/srv/stack/preprod-stack.json",
  [JOURNEY_DEPLOYMENT_ENV.stackRunId]: "drill-1",
  [JOURNEY_DEPLOYMENT_ENV.operator2ComposeProject]: "midgard-node-op2",
  [JOURNEY_DEPLOYMENT_ENV.operator2Endpoint]: "http://127.0.0.1:3100",
};

describe("journey deployment source", () => {
  it("selects nothing without a devnet run directory, so the journey skips", () => {
    expect(selectJourneyDeploymentSource({})).toBeUndefined();
    expect(
      selectJourneyDeploymentSource({
        [JOURNEY_DEPLOYMENT_ENV.mode]: "devnet",
      }),
    ).toBeUndefined();
  });

  it("defaults to the devnet run directory, explicitly or not", async () => {
    const runDirectory = await emptyDirectory();
    for (const env of [
      { [JOURNEY_DEPLOYMENT_ENV.runDirectory]: runDirectory },
      {
        [JOURNEY_DEPLOYMENT_ENV.mode]: "devnet",
        [JOURNEY_DEPLOYMENT_ENV.runDirectory]: runDirectory,
      },
    ]) {
      const source = selectJourneyDeploymentSource(env);
      expect(source).toMatchObject({ mode: "devnet", runDirectory });
    }
  });

  it("opens a devnet run directory exactly as the devnet journey always has", async () => {
    for (const runDirectory of [await emptyDirectory(), "relative/run", ""]) {
      const source = selectJourneyDeploymentSource({
        [JOURNEY_DEPLOYMENT_ENV.runDirectory]: runDirectory,
      });
      expect(source?.mode).toBe("devnet");
      expect(await failureOf(() => source!.open())).toBe(
        await failureOf(() => loadJourneyContext(runDirectory)),
      );
    }
  });

  it("selects a stack by its absolute configuration file, run id and operator-2 node", () => {
    expect(selectJourneyDeploymentSource(stackEnv)).toMatchObject({
      mode: "stack",
      configPath: stackEnv[JOURNEY_DEPLOYMENT_ENV.stackConfig],
      runId: stackEnv[JOURNEY_DEPLOYMENT_ENV.stackRunId],
      operator2: {
        composeProject: "midgard-node-op2",
        endpoint: "http://127.0.0.1:3100",
      },
    });
  });

  it("refuses a stack without an absolute configuration file, a run id or operator-2's node", () => {
    for (const env of [
      { ...stackEnv, [JOURNEY_DEPLOYMENT_ENV.stackConfig]: "" },
      { ...stackEnv, [JOURNEY_DEPLOYMENT_ENV.stackConfig]: "stack.json" },
      { ...stackEnv, [JOURNEY_DEPLOYMENT_ENV.stackRunId]: "" },
      { ...stackEnv, [JOURNEY_DEPLOYMENT_ENV.operator2ComposeProject]: "" },
      {
        ...stackEnv,
        [JOURNEY_DEPLOYMENT_ENV.operator2ComposeProject]: "Op2 --file x",
      },
      { ...stackEnv, [JOURNEY_DEPLOYMENT_ENV.operator2Endpoint]: "" },
      {
        ...stackEnv,
        [JOURNEY_DEPLOYMENT_ENV.operator2Endpoint]: "127.0.0.1:3100",
      },
      {
        ...stackEnv,
        [JOURNEY_DEPLOYMENT_ENV.operator2Endpoint]:
          "http://127.0.0.1:3100/readyz",
      },
    ])
      expect(() => selectJourneyDeploymentSource(env)).toThrow(/Stack mode/u);
  });

  it("refuses a mixed or unknown mode instead of guessing", () => {
    expect(() =>
      selectJourneyDeploymentSource({
        ...stackEnv,
        [JOURNEY_DEPLOYMENT_ENV.runDirectory]: "/srv/devnet",
      }),
    ).toThrow(JOURNEY_DEPLOYMENT_ENV.runDirectory);
    for (const name of [
      JOURNEY_DEPLOYMENT_ENV.stackConfig,
      JOURNEY_DEPLOYMENT_ENV.stackRunId,
      JOURNEY_DEPLOYMENT_ENV.operator2ComposeProject,
      JOURNEY_DEPLOYMENT_ENV.operator2Endpoint,
    ])
      expect(() =>
        selectJourneyDeploymentSource({
          [JOURNEY_DEPLOYMENT_ENV.runDirectory]: "/srv/devnet",
          [name]: stackEnv[name],
        }),
      ).toThrow(name);
    expect(() =>
      selectJourneyDeploymentSource({
        [JOURNEY_DEPLOYMENT_ENV.mode]: "preprod",
      }),
    ).toThrow(JOURNEY_DEPLOYMENT_ENV.mode);
  });

  it("opens a stack through the stack's own configuration loader", async () => {
    const missing = join(await emptyDirectory(), "missing-stack.json");
    const source = selectJourneyDeploymentSource({
      ...stackEnv,
      [JOURNEY_DEPLOYMENT_ENV.stackConfig]: missing,
    });
    expect(await failureOf(() => source!.open())).toContain(missing);
  });
});
