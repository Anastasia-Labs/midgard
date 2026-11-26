import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk/core";
import { describe, expect, it } from "vitest";

import { main } from "../src/bin.js";
import { usage } from "../src/bin.parsed-args.js";
import {
  ADMITTED_SUBMISSION_COMMANDS,
  ADMITTED_SUBMIT_INIT_CATEGORIES,
  rejectUnadmittedSubmissionRoute,
  UNADMITTED_SUBMISSION_ROUTE_ERROR_PREFIX,
} from "../src/submission-route-admission.js";
import { submitInitFromFiles } from "../src/submit-init.submit-init.js";

describe("submission route admission", () => {
  it("does not publish the diagnostic step builders from the package barrel", async () => {
    const exports = await import("../src/index.js");
    for (const name of [
      "neSubmitStep01",
      "neSubmitStep02",
      "neSubmitStep03",
      "neSubmitStep04",
      "buildInit",
      "submitInitFromFiles",
      "buildStep01",
      "buildStep02",
      "buildStep03",
      "buildStep04",
      "buildInvalidRangeStep01",
      "buildZeroInputStep01",
    ]) {
      expect(Object.prototype.hasOwnProperty.call(exports, name)).toBe(false);
    }
    for (const name of [
      "prepareDoubleSpendFromFile",
      "buildTransitionTraceProof",
      "submitRemoveFraudulentBlock",
    ]) {
      expect(Object.prototype.hasOwnProperty.call(exports, name)).toBe(true);
    }
  });

  it("admits exactly the routed submission commands and only catalogue categories", () => {
    // The CLI's own usage text names every routed command.
    const routed = new Set(
      [
        ...usage.matchAll(
          /midgard-fault-proofs ((?:submit|remove)-[a-z0-9-]+)/g,
        ),
      ].map(([, command]) => command!),
    );
    expect(new Set(ADMITTED_SUBMISSION_COMMANDS)).toEqual(routed);
    expect(ADMITTED_SUBMISSION_COMMANDS).toHaveLength(routed.size);
    const catalogue = new Set<string>(FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER);
    for (const fraudCategory of ADMITTED_SUBMIT_INIT_CATEGORIES) {
      expect(catalogue.has(fraudCategory)).toBe(true);
    }
    expect(ADMITTED_SUBMIT_INIT_CATEGORIES).toHaveLength(
      new Set(ADMITTED_SUBMIT_INIT_CATEGORIES).size,
    );
  });

  it("refuses an unadmitted route or init category with its own error and passes every admitted one", () => {
    for (const command of ["submit-unknown-route", "remove-unknown-block"]) {
      expect(() => rejectUnadmittedSubmissionRoute({ command })).toThrow(
        `${UNADMITTED_SUBMISSION_ROUTE_ERROR_PREFIX}: ${command}`,
      );
    }
    expect(() =>
      rejectUnadmittedSubmissionRoute({
        command: "submit-init",
        fraudCategory: "notACategory",
      }),
    ).toThrow(
      `${UNADMITTED_SUBMISSION_ROUTE_ERROR_PREFIX}: submit-init fraudCategory=notACategory`,
    );
    for (const command of ADMITTED_SUBMISSION_COMMANDS) {
      expect(() =>
        rejectUnadmittedSubmissionRoute({
          command,
          fraudCategory: "transitionTrace",
        }),
      ).not.toThrow();
    }
    for (const fraudCategory of ADMITTED_SUBMIT_INIT_CATEGORIES) {
      expect(() =>
        rejectUnadmittedSubmissionRoute({
          command: "submit-init",
          fraudCategory,
        }),
      ).not.toThrow();
    }
    for (const command of ["prepare-double-spend", "inspect-contracts"]) {
      expect(() => rejectUnadmittedSubmissionRoute({ command })).not.toThrow();
    }
  });

  it("rejects an unadmitted CLI command or init category before reading its blueprint", async () => {
    const previousArgv = process.argv;
    const missingFiles = [
      "--blueprint",
      "/nonexistent/blueprint.json",
      "--deployment-info",
      "/nonexistent/deployment-info.json",
    ];
    try {
      for (const command of ["submit-unknown-route", "remove-unknown-block"]) {
        process.argv = [
          "node",
          "midgard-fault-proofs",
          command,
          ...missingFiles,
        ];
        await expect(main()).rejects.toThrow(
          `${UNADMITTED_SUBMISSION_ROUTE_ERROR_PREFIX}: ${command}`,
        );
      }
    } finally {
      process.argv = previousArgv;
    }
    await expect(
      submitInitFromFiles({
        blueprintPath: "/nonexistent/blueprint.json",
        deploymentInfoPath: "/nonexistent/deployment-info.json",
        fraudCategory: "notACategory",
      } as never),
    ).rejects.toThrow(
      `${UNADMITTED_SUBMISSION_ROUTE_ERROR_PREFIX}: submit-init fraudCategory=notACategory`,
    );
  });

  it("routes every admitted CLI command past the gate to its own argument checks", async () => {
    const previousArgv = process.argv;
    try {
      for (const command of ADMITTED_SUBMISSION_COMMANDS) {
        process.argv = [
          "node",
          "midgard-fault-proofs",
          command,
          "--fraud-category",
          "transitionTrace",
        ];
        await expect(main()).rejects.toThrow(/^Missing required --/);
      }
    } finally {
      process.argv = previousArgv;
    }
  });
});
