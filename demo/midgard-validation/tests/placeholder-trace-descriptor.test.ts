import {
  decodeMidgardValidationTraceDescriptor,
  encodeMidgardValidationTraceDescriptor,
} from "@al-ft/midgard-core/validation-trace";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  placeholderValidationTraceDescriptorV1,
  validationTransactionCommitmentV1,
} from "../src/validation-machine/placeholder-trace-descriptor.js";
import {
  PLACEHOLDER_VALIDATION_TRACE_DESCRIPTOR_VECTORS,
  placeholderValidationTraceDescriptorVector,
} from "./placeholder-validation-trace-descriptor-v1.vectors.js";

const hex = (value: string): Buffer => Buffer.from(value, "hex");

describe("placeholderValidationTraceDescriptorV1", () => {
  it.each(PLACEHOLDER_VALIDATION_TRACE_DESCRIPTOR_VECTORS)(
    "reproduces the $name golden vector",
    (vector) => {
      const descriptor = placeholderValidationTraceDescriptorV1(
        {
          eventKeyCbor: hex(vector.eventKeyCbor),
          transactionCommitment: hex(vector.transactionCommitment),
        },
        vector.verdict,
        vector.rejectionReason,
      );
      expect({
        schemaVersion: descriptor.schemaVersion,
        machineVersion: descriptor.machineVersion,
        stepCount: descriptor.stepCount,
        traceRoot: descriptor.traceRoot.toString("hex"),
        initialStateHash: descriptor.initialStateHash.toString("hex"),
        terminalStateHash: descriptor.terminalStateHash.toString("hex"),
        rejectionCodeHash: descriptor.rejectionCodeHash.toString("hex"),
      }).toEqual({
        schemaVersion: vector.expected.schemaVersion,
        machineVersion: vector.expected.machineVersion,
        stepCount: vector.expected.stepCount,
        traceRoot: vector.expected.traceRoot,
        initialStateHash: vector.expected.initialStateHash,
        terminalStateHash: vector.expected.terminalStateHash,
        rejectionCodeHash: vector.expected.rejectionCodeHash,
      });
      expect(descriptor.verdict).toBe(vector.verdict);
      // The bytes committed under the validation_traces root.
      expect(
        LucidData.to(
          SDK.validationTraceDescriptorDataFromCore(descriptor),
          SDK.ValidationTraceDescriptor,
        ),
      ).toBe(vector.expected.descriptorDataCbor);
      // The core codec round-trips it, so every shape-only reader accepts it.
      expect(
        decodeMidgardValidationTraceDescriptor(
          encodeMidgardValidationTraceDescriptor(descriptor),
        ),
      ).toEqual(descriptor);
    },
  );

  it("distinguishes the event, the transaction and the verdict", () => {
    const accepted =
      placeholderValidationTraceDescriptorVector("normal-accepted");
    const rejected =
      placeholderValidationTraceDescriptorVector("forced-rejected");
    const otherCode = placeholderValidationTraceDescriptorVector(
      "forced-rejected-min-fee",
    );
    expect(
      new Set(
        PLACEHOLDER_VALIDATION_TRACE_DESCRIPTOR_VECTORS.map(
          (vector) => vector.expected.traceRoot,
        ),
      ).size,
    ).toBe(PLACEHOLDER_VALIDATION_TRACE_DESCRIPTOR_VECTORS.length);
    expect(rejected.expected.initialStateHash).toBe(
      otherCode.expected.initialStateHash,
    );
    expect(rejected.expected.terminalStateHash).not.toBe(
      otherCode.expected.terminalStateHash,
    );
    const changedCommitment = placeholderValidationTraceDescriptorV1(
      {
        eventKeyCbor: hex(accepted.eventKeyCbor),
        transactionCommitment: Buffer.alloc(32, 0x12),
      },
      "accepted",
      null,
    );
    expect(changedCommitment.initialStateHash.toString("hex")).not.toBe(
      accepted.expected.initialStateHash,
    );
    expect(changedCommitment.terminalStateHash.toString("hex")).toBe(
      accepted.expected.terminalStateHash,
    );
  });

  it("derives the vector commitments from the transaction bytes", () => {
    const withTransactions =
      PLACEHOLDER_VALIDATION_TRACE_DESCRIPTOR_VECTORS.filter(
        (vector) => vector.transaction !== undefined,
      );
    expect(
      withTransactions.map(({ transaction }) => transaction!.sourceKind),
    ).toEqual(["normal", "forced"]);
    for (const vector of withTransactions) {
      expect(
        validationTransactionCommitmentV1({
          canonicalTransactionCbor: hex(
            vector.transaction!.canonicalTransactionCbor,
          ),
          sourceKind: vector.transaction!.sourceKind,
        }).toString("hex"),
      ).toBe(vector.transactionCommitment);
    }
  });

  it("refuses a verdict that disagrees with its rejection reason", () => {
    const event = {
      eventKeyCbor: hex(
        PLACEHOLDER_VALIDATION_TRACE_DESCRIPTOR_VECTORS[0]!.eventKeyCbor,
      ),
      transactionCommitment: Buffer.alloc(32, 1),
    };
    expect(() =>
      placeholderValidationTraceDescriptorV1(
        event,
        "accepted",
        "FeeBelowMinimum",
      ),
    ).toThrow();
    expect(() =>
      placeholderValidationTraceDescriptorV1(event, "rejected", null),
    ).toThrow();
    expect(() =>
      placeholderValidationTraceDescriptorV1(
        { ...event, transactionCommitment: Buffer.alloc(31) },
        "accepted",
        null,
      ),
    ).toThrow();
  });
});
