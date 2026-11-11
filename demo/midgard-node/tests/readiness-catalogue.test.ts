/**
 * The readiness catalogue (`src/services/readiness-catalogue*.ts`): its rows
 * are well formed, a public profile shows a line as the catalogue bounds it,
 * and the textual reading `scripts/ci/check-readiness-reasons-doc.mjs` makes
 * of it (where the node's source cannot be imported) is the catalogue.
 */
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import { cataloguedCodes } from "../../../scripts/ci/check-readiness-reasons-doc.mjs";
import { publicReadinessPresentation } from "../src/commands/listen-readiness-public-presentation.js";
import {
  parseReadinessLine,
  publicReadinessLine,
  READINESS_CATALOGUE,
} from "../src/services/readiness-catalogue.js";

const REPO_ROOT = fileURLToPath(new URL("../../..", import.meta.url));
const REDACTED = "E_READINESS_DIAGNOSTIC_REDACTED";
const textRows = READINESS_CATALOGUE.filter((row) =>
  row.params.some((param) => param.kind === "text"),
);

describe("the readiness catalogue", () => {
  it("holds each code once, free text last and optional parameters trailing", () => {
    const codes = READINESS_CATALOGUE.map((row) => row.code);
    expect(new Set(codes).size).toBe(codes.length);
    for (const { code, params } of READINESS_CATALOGUE) {
      const text = params.findIndex((param) => param.kind === "text");
      expect(text === -1 || text === params.length - 1, code).toBe(true);
      const firstOptional = params.findIndex(
        (param) => param.kind === "bounded" && param.optional,
      );
      if (firstOptional >= 0)
        expect(
          params
            .slice(firstOptional)
            .every((param) => param.kind === "bounded" && param.optional),
          code,
        ).toBe(true);
    }
  });

  it("is what the reasons-doc check reads from its source", () => {
    expect(cataloguedCodes(REPO_ROOT)).toEqual(
      new Map(READINESS_CATALOGUE.map((row) => [row.code, row.kind])),
    );
  });

  it.each(["preprod-public", "mainnet"])(
    "shows every free-text row as its code alone in %s",
    (profile) => {
      expect(textRows.map((row) => row.code)).toEqual(
        expect.arrayContaining([
          "forced_admission_divergence",
          "settlement_reconcile_stuck",
        ]),
      );
      const secret = "upstream text c2VjcmV0:with:colons";
      const lines = textRows.map(({ code, params }) =>
        [
          code,
          ...params.map((param) => (param.kind === "text" ? secret : "1")),
        ].join(":"),
      );
      for (const line of lines) expect(parseReadinessLine(line)).toBeDefined();
      const codes = textRows.map((row) => row.code);
      expect(
        publicReadinessPresentation(
          { reasons: lines, details: lines, l1Follower: { reason: lines[0] } },
          profile,
          {},
        ),
      ).toEqual({
        reasons: codes,
        details: codes,
        l1Follower: { reason: codes[0] },
      });
    },
  );

  it("shows a bounded line as it is, and a value outside its format not at all", () => {
    for (const line of [
      "stale_heartbeat:merge:1200",
      "state_queue_lease_held_long:block_commitment:600001",
      `validation-trace-defense-underfunded:${"ab".repeat(28)}:5000:2000000`,
      `validation-trace-defense-awaiting:${"ab".repeat(28)}:none`,
      "commit_da_frame_pressure:required_work:90",
      "native_mpf_full_index_near_cap:FULL_INDEX_MAX_BYTES:9:10",
      "commit_build_held:phase_b",
    ]) {
      expect(parseReadinessLine(line), line).toBeDefined();
      expect(publicReadinessLine(line)).toBe(line);
    }
    expect(publicReadinessLine("commit_build_held:SECRET")).toBe(
      "commit_build_held:unclassified",
    );
    for (const [line, shown] of [
      ["stale_heartbeat:SECRET:1200", "stale_heartbeat"],
      ["db_unhealthy:SECRET", "db_unhealthy"],
      ["queue_depth_exceeded:1", "queue_depth_exceeded"],
      [
        `validation-trace-defense-late:${"ab".repeat(28)}:9:1:2`,
        "validation-trace-defense-late",
      ],
    ]) {
      expect(parseReadinessLine(line), line).toBeUndefined();
      expect(publicReadinessLine(line)).toBe(shown);
    }
  });

  it.each(["preprod-public", "mainnet"])(
    "redacts a code outside the catalogue in %s",
    (profile) => {
      for (const code of ["source_timeout", "not_a_readiness_code:1"])
        expect(
          publicReadinessPresentation({ reasons: [code] }, profile, {}),
        ).toEqual({ reasons: [REDACTED] });
    },
  );
});
