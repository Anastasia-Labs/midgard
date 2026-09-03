import { describe, expect, it } from "vitest";

import { main } from "../src/bin.js";
import {
  isRetiredUnauthenticatedSubmissionRoute,
  rejectRetiredUnauthenticatedSubmissionRoute,
  RETIRED_UNAUTHENTICATED_SUBMISSION_ERROR_PREFIX,
} from "../src/legacy-submission-boundary-v1.js";
import { neSubmitStep01FromFiles } from "../src/ne-submit-step-01.js";
import { neSubmitStep02FromFiles } from "../src/ne-submit-step-02.js";
import { neSubmitStep03FromFiles } from "../src/ne-submit-step-03.js";
import { neSubmitStep04FromFiles } from "../src/ne-submit-step-04.js";
import { submitInitFromFiles } from "../src/submit-init.js";
import { submitInvalidRangeStep01FromFiles } from "../src/submit-invalid-range-step-01.js";
import { submitInvalidRangeStep02FromFiles } from "../src/submit-invalid-range-step-02.js";
import { submitStep01FromFiles } from "../src/submit-step-01.js";
import { submitStep02FromFiles } from "../src/submit-step-02.js";
import { submitStep03FromFiles } from "../src/submit-step-03.js";
import { submitStep04FromFiles } from "../src/submit-step-04.js";
import { submitZeroInputStep01FromFiles } from "../src/submit-zero-input-step-01.js";
import { submitZeroInputStep02FromFiles } from "../src/submit-zero-input-step-02.js";

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

describe("RF-043 legacy submission boundary", () => {
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
      "neSubmitStep01FromFiles",
      "neSubmitStep02",
      "neSubmitStep02FromFiles",
      "neSubmitStep03",
      "neSubmitStep03FromFiles",
      "neSubmitStep04",
      "neSubmitStep04FromFiles",
      "submitInit",
      "submitInitFromFiles",
      "submitInvalidRangeStep01FromFiles",
      "submitStep01",
      "submitStep01FromFiles",
      "submitInvalidRangeStep02",
      "submitInvalidRangeStep02FromFiles",
      "submitStep02",
      "submitStep02FromFiles",
      "submitStep03",
      "submitStep03FromFiles",
      "submitStep04",
      "submitStep04FromFiles",
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
        "submitTransitionTraceProof",
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
      ["submit-step-01", () => submitStep01FromFiles({} as never)],
      ["submit-step-02", () => submitStep02FromFiles({} as never)],
      ["submit-step-03", () => submitStep03FromFiles({} as never)],
      ["submit-step-04", () => submitStep04FromFiles({} as never)],
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
      [
        "submit-non-existent-input-step-01",
        () => neSubmitStep01FromFiles({} as never),
      ],
      [
        "submit-non-existent-input-step-02",
        () => neSubmitStep02FromFiles({} as never),
      ],
      [
        "submit-non-existent-input-step-03",
        () => neSubmitStep03FromFiles({} as never),
      ],
      [
        "submit-non-existent-input-step-04",
        () => neSubmitStep04FromFiles({} as never),
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
});
