import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk/core";
import { describe, expect, it } from "vitest";

import { main } from "../src/bin.js";
import { usage } from "../src/bin.parsed-args.js";
import {
  ADMITTED_SUBMISSION_COMMANDS,
  ADMITTED_SUBMIT_INIT_CATEGORIES,
  isRetiredUnauthenticatedSubmissionRoute,
  rejectRetiredUnauthenticatedSubmissionRoute,
  rejectUnadmittedSubmissionRoute,
  RETIRED_UNAUTHENTICATED_SUBMISSION_ERROR_PREFIX,
  UNADMITTED_SUBMISSION_ROUTE_ERROR_PREFIX,
} from "../src/legacy-submission-boundary.js";
import { submitInitFromFiles } from "../src/submit-init.submit-init.js";
import { submitInvalidRangeStep01FromFiles } from "../src/submit-invalid-range-step-01.submit-invalid-range-step01-from-files.js";
import { submitInvalidRangeStep02FromFiles } from "../src/submit-invalid-range-step-02.submit-invalid-range-step02-v1.js";
import { submitZeroInputStep01FromFiles } from "../src/submit-zero-input-step-01.submit-zero-input-step01-from-files.js";
import { submitZeroInputStep02FromFiles } from "../src/submit-zero-input-step-02.submit-zero-input-step02-v1.js";

const retiredCommands = [
  "submit-step-01",
  "submit-step-02",
  "submit-step-03",
  "submit-step-04",
  "submit-invalid-range-step-01",
  "submit-invalid-range-step-02",
  "submit-zero-input-step-01",
  "submit-zero-input-step-02",
  "submit-non-existent-input-step-01",
  "submit-non-existent-input-step-02",
  "submit-non-existent-input-step-03",
  "submit-non-existent-input-step-04",
] as const;

const retiredInitCategories = [
  "doubleSpend",
  "invalidRange",
  "zeroInput",
  "nonExistentInput",
  "nonExistentInputNoIndex",
] as const;

describe("legacy submission boundary", () => {
  it("covers every retired command and leaves canonical controls available", () => {
    for (const command of retiredCommands) {
      expect(isRetiredUnauthenticatedSubmissionRoute({ command })).toBe(true);
    }
    for (const fraudCategory of retiredInitCategories) {
      expect(
        isRetiredUnauthenticatedSubmissionRoute({
          command: "submit-init",
          fraudCategory,
        }),
      ).toBe(true);
    }
    expect(
      isRetiredUnauthenticatedSubmissionRoute({ command: "submit-init" }),
    ).toBe(true);
    expect(
      isRetiredUnauthenticatedSubmissionRoute({
        command: "submit-init",
        fraudCategory: "transitionTrace",
      }),
    ).toBe(false);
    expect(
      isRetiredUnauthenticatedSubmissionRoute({
        command: "submit-init",
        fraudCategory: "validationTraceDispute",
      }),
    ).toBe(false);
    expect(
      isRetiredUnauthenticatedSubmissionRoute({
        command: "prepare-transition-trace",
      }),
    ).toBe(false);
    expect(
      isRetiredUnauthenticatedSubmissionRoute({
        command: "submit-transition-trace-proof",
      }),
    ).toBe(false);
  });

  it("uses one deterministic error for the retired command surface", () => {
    expect(() =>
      rejectRetiredUnauthenticatedSubmissionRoute({
        command: "submit-step-01",
      }),
    ).toThrow(
      `${RETIRED_UNAUTHENTICATED_SUBMISSION_ERROR_PREFIX}: submit-step-01`,
    );
  });

  it("does not publish retired submit-init/submit-step functions from the package barrel", async () => {
    const exports = await import("../src/index.js");
    for (const name of [
      "neSubmitStep01",
      "neSubmitStep02",
      "neSubmitStep03",
      "neSubmitStep04",
      "submitInit",
      "submitInitFromFiles",
      "submitInvalidRangeStep01FromFiles",
      "submitStep01",
      "submitInvalidRangeStep02",
      "submitInvalidRangeStep02FromFiles",
      "submitStep02",
      "submitStep03",
      "submitStep04",
      "submitInvalidRangeStep01",
      "submitZeroInputStep01",
      "submitZeroInputStep01FromFiles",
      "submitZeroInputStep02",
      "submitZeroInputStep02FromFiles",
    ]) {
      expect(Object.prototype.hasOwnProperty.call(exports, name)).toBe(false);
    }
    expect(
      Object.prototype.hasOwnProperty.call(
        exports,
        "prepareDoubleSpendFromFile",
      ),
    ).toBe(true);
    expect(
      Object.prototype.hasOwnProperty.call(
        exports,
        "buildTransitionTraceProof",
      ),
    ).toBe(true);
    expect(
      Object.prototype.hasOwnProperty.call(
        exports,
        "submitRemoveFraudulentBlock",
      ),
    ).toBe(true);
  });

  it("rejects every file entrypoint before it can read config or construct Lucid", async () => {
    const fileEntrypoints: readonly [string, () => Promise<unknown>][] = [
      ["submit-init", () => submitInitFromFiles({} as never)],
      [
        "submit-invalid-range-step-01",
        () => submitInvalidRangeStep01FromFiles({} as never),
      ],
      [
        "submit-invalid-range-step-02",
        () => submitInvalidRangeStep02FromFiles({} as never),
      ],
      [
        "submit-zero-input-step-01",
        () => submitZeroInputStep01FromFiles({} as never),
      ],
      [
        "submit-zero-input-step-02",
        () => submitZeroInputStep02FromFiles({} as never),
      ],
    ];

    for (const [command, invoke] of fileEntrypoints) {
      await expect(invoke()).rejects.toThrow(
        `${RETIRED_UNAUTHENTICATED_SUBMISSION_ERROR_PREFIX}: ${command}`,
      );
    }
  });

  it("rejects every retired CLI command before blueprint/provider/wallet side effects", async () => {
    const previousArgv = process.argv;
    try {
      for (const command of retiredCommands) {
        process.argv = ["node", "midgard-fault-proofs", command];
        await expect(main()).rejects.toThrow(
          `${RETIRED_UNAUTHENTICATED_SUBMISSION_ERROR_PREFIX}: ${command}`,
        );
      }
      for (const fraudCategory of retiredInitCategories) {
        process.argv = [
          "node",
          "midgard-fault-proofs",
          "submit-init",
          "--fraud-category",
          fraudCategory,
        ];
        await expect(main()).rejects.toThrow(
          `${RETIRED_UNAUTHENTICATED_SUBMISSION_ERROR_PREFIX}: submit-init fraudCategory=${fraudCategory}`,
        );
      }
      process.argv = ["node", "midgard-fault-proofs", "submit-init"];
      await expect(main()).rejects.toThrow(
        `${RETIRED_UNAUTHENTICATED_SUBMISSION_ERROR_PREFIX}: submit-init`,
      );

      process.argv = [
        "node",
        "midgard-fault-proofs",
        "submit-init",
        "--fraud-category",
        "transitionTrace",
      ];
      await expect(main()).rejects.toThrow("Missing required --blueprint");
    } finally {
      process.argv = previousArgv;
    }
  });

  it("admits exactly the routed submission commands and init categories that are not retired", () => {
    // The CLI's own usage text names every routed command.
    const routed = new Set(
      [
        ...usage.matchAll(
          /midgard-fault-proofs ((?:submit|remove)-[a-z0-9-]+)/g,
        ),
      ].map(([, command]) => command!),
    );
    // submit-init is classified by its category; with an admitted one it is
    // admitted.
    const retired = [...routed].filter((command) =>
      isRetiredUnauthenticatedSubmissionRoute({
        command,
        fraudCategory: "transitionTrace",
      }),
    );
    expect(new Set(retired)).toEqual(new Set(retiredCommands));
    expect(new Set(ADMITTED_SUBMISSION_COMMANDS)).toEqual(
      new Set([...routed].filter((command) => !retired.includes(command))),
    );
    expect(ADMITTED_SUBMISSION_COMMANDS).toHaveLength(
      new Set(ADMITTED_SUBMISSION_COMMANDS).size,
    );

    const retiredCategories = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.filter(
      (fraudCategory) =>
        isRetiredUnauthenticatedSubmissionRoute({
          command: "submit-init",
          fraudCategory,
        }),
    );
    expect(retiredCategories).toHaveLength(17);
    expect(new Set(ADMITTED_SUBMIT_INIT_CATEGORIES)).toEqual(
      new Set(
        FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.filter(
          (fraudCategory) => !retiredCategories.includes(fraudCategory),
        ),
      ),
    );
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
    for (const command of retiredCommands) {
      expect(() => rejectUnadmittedSubmissionRoute({ command })).toThrow(
        `${RETIRED_UNAUTHENTICATED_SUBMISSION_ERROR_PREFIX}: ${command}`,
      );
    }
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
