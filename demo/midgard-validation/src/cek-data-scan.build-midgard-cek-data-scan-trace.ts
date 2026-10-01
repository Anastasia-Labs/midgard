import {
  buildMidgardValidationMerkleFrontier,
  buildMidgardValidationMerkleMembership,
} from "@al-ft/midgard-core";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
} from "@harmoniclabs/plutus-data";

import { encodeMidgardCekPlutusData } from "./cek-constant.js";
import {
  appendChild,
  constructorHeaderLength,
  frameWith,
  hashMidgardCekDataScanChild,
  mapHeaderLength,
  type MutableFrame,
  replaceFrame,
  scalarSummary,
  type ScanWork,
  type StructuredData,
  structuredSummary,
} from "./cek-data-scan.scalar-summary.js";
import {
  hash32,
  hashMidgardCekDataScanFrame,
  type MidgardCekDataScanControl,
  type MidgardCekDataScanStep,
  type MidgardCekDataScanTraceStep,
} from "./cek-data-scan.validate-midgard-cek-data-scan-frame.js";
import { plutusDataFromCborIterative } from "./plutus-data-iterative.decode.js";
import { isPlutusDataMap } from "./plutus-data-narrowing.js";
import {
  emptyMidgardCekDataListSummary,
  emptyMidgardCekDataPairSummary,
  prependMidgardCekDataListSummary,
  prependMidgardCekDataPairSummary,
} from "./script-context-proof.js";

/**
 * Produces the exact content-addressed scan accepted by the L1 Data scanner.
 * Every transition reveals at most the one independently bounded raw Data
 * preimage plus fixed-size frame/frontier material.
 */
export const buildMidgardCekDataScanTrace = (
  rawCbor: Uint8Array,
): {
  readonly initial: MidgardCekDataScanControl;
  readonly steps: readonly MidgardCekDataScanTraceStep[];
  readonly terminal: MidgardCekDataScanControl;
} => {
  const raw = Buffer.from(rawCbor);
  if (raw.length === 0 || raw.length > 9_215) {
    throw new Error("V1 Data scan preimage must contain 1..9215 bytes");
  }
  const rootData = plutusDataFromCborIterative(raw);
  const initial: MidgardCekDataScanControl = {
    rawHash: hash32(raw),
    rawLength: raw.length,
    offset: 0,
    frameRoot: Buffer.alloc(0),
    frameClosed: false,
    result: null,
  };
  let control = initial;
  const steps: MidgardCekDataScanTraceStep[] = [];
  const emit = (step: MidgardCekDataScanStep): void => {
    steps.push({ control, step });
  };

  const canonical = encodeMidgardCekPlutusData(rootData);
  if (!canonical.equals(raw)) {
    throw new Error("V1 Data scan source is not canonical Data CBOR");
  }
  const work: ScanWork[] = [{ kind: "enter", data: rootData, parent: null }];
  while (work.length > 0) {
    const operation = work.pop()!;
    if (operation.kind === "enter") {
      const { data, parent } = operation;
      if (data instanceof DataI || data instanceof DataB) {
        const summary = scalarSummary(data);
        const encoded = encodeMidgardCekPlutusData(data);
        if (data instanceof DataB && data.bytes.length > 9_215) {
          throw new Error(
            "CEK Data scanner byte leaf exceeds its proof envelope",
          );
        }
        emit({
          kind: "revealLeaf",
          rawCbor: raw,
          parent: parent?.frame ?? null,
          itemLength: encoded.length,
        });
        control = {
          ...control,
          offset: control.offset + encoded.length,
        };
        if (parent === null) {
          if (control.offset !== raw.length) {
            throw new Error("CEK scalar Data root has trailing bytes");
          }
          control = { ...control, result: summary };
          continue;
        }
        const nextParent = appendChild(parent, summary);
        replaceFrame(parent, nextParent);
        control = {
          ...control,
          frameRoot: hashMidgardCekDataScanFrame(parent.frame),
          frameClosed:
            parent.frame.kind === 3 &&
            parent.frame.childCount === parent.frame.expectedChildren,
        };
        continue;
      }

      if (
        !(data instanceof DataConstr) &&
        !(data instanceof DataList) &&
        !isPlutusDataMap(data)
      ) {
        throw new Error("CEK Data scanner received an unknown node");
      }
      if (data instanceof DataConstr && data.constr < 0n) {
        throw new Error("Plutus Data constructor must be non-negative");
      }
      const structuredData: StructuredData = data;
      const children: Data[] =
        data instanceof DataConstr
          ? [...data.fields]
          : data instanceof DataList
            ? [...data.list]
            : data.map.flatMap((pair) => [pair.fst, pair.snd]);
      const kind: 1 | 2 | 3 =
        data instanceof DataConstr ? 1 : data instanceof DataList ? 2 : 3;
      const constructor = data instanceof DataConstr ? data.constr : 0n;
      const frame: MutableFrame = {
        frame: {
          kind,
          constructor,
          tail:
            parent === null
              ? Buffer.alloc(0)
              : hashMidgardCekDataScanFrame(parent.frame),
          expectedChildren: children.length,
          childCount: 0,
          childFrontier: buildMidgardValidationMerkleFrontier([]),
          foldCursor: 0,
          sequence:
            kind === 3
              ? emptyMidgardCekDataPairSummary()
              : emptyMidgardCekDataListSummary(),
        },
        children: [],
      };
      if (kind === 1) {
        emit({
          kind: "openConstructor",
          rawCbor: raw,
          parent: parent?.frame ?? null,
          constructor,
          expectedChildren: children.length,
        });
        control = {
          ...control,
          offset: control.offset + constructorHeaderLength(constructor),
        };
      } else if (kind === 2) {
        emit({
          kind: "openList",
          rawCbor: raw,
          parent: parent?.frame ?? null,
          expectedChildren: children.length,
        });
        control = { ...control, offset: control.offset + 1 };
      } else {
        emit({
          kind: "openMap",
          rawCbor: raw,
          parent: parent?.frame ?? null,
        });
        control = {
          ...control,
          offset: control.offset + mapHeaderLength(children.length / 2),
        };
      }
      control = {
        ...control,
        frameRoot: hashMidgardCekDataScanFrame(frame.frame),
        frameClosed: children.length === 0,
      };
      work.push({ kind: "exit", data: structuredData, frame, parent });
      for (let index = children.length - 1; index >= 0; index -= 1) {
        work.push({ kind: "enter", data: children[index]!, parent: frame });
      }
      continue;
    }

    const { data, frame, parent } = operation;
    if (frame.frame.kind !== 3 && frame.frame.expectedChildren > 0) {
      emit({ kind: "closeSequence", rawCbor: raw, frame: frame.frame });
      control = {
        ...control,
        offset: control.offset + 1,
        frameClosed: true,
      };
    }

    const leaves = frame.children.map((child, index) =>
      hashMidgardCekDataScanChild(index, child),
    );
    if (frame.frame.kind === 3) {
      for (
        let pairIndex = frame.frame.expectedChildren / 2 - 1;
        pairIndex >= 0;
        pairIndex -= 1
      ) {
        const keyIndex = pairIndex * 2;
        const valueIndex = keyIndex + 1;
        const key = frame.children[keyIndex]!;
        const value = frame.children[valueIndex]!;
        emit({
          kind: "foldMap",
          frame: frame.frame,
          pairIndex,
          key,
          value,
          keySiblings: buildMidgardValidationMerkleMembership(leaves, keyIndex)
            .siblings,
          valueSiblings: buildMidgardValidationMerkleMembership(
            leaves,
            valueIndex,
          ).siblings,
        });
        replaceFrame(
          frame,
          frameWith(
            frame,
            frame.frame.foldCursor + 1,
            prependMidgardCekDataPairSummary(key, value, frame.frame.sequence),
          ),
        );
        control = {
          ...control,
          frameRoot: hashMidgardCekDataScanFrame(frame.frame),
        };
      }
    } else {
      for (
        let childIndex = frame.frame.expectedChildren - 1;
        childIndex >= 0;
        childIndex -= 1
      ) {
        const child = frame.children[childIndex]!;
        emit({
          kind: "foldList",
          frame: frame.frame,
          childIndex,
          child,
          siblings: buildMidgardValidationMerkleMembership(leaves, childIndex)
            .siblings,
        });
        replaceFrame(
          frame,
          frameWith(
            frame,
            frame.frame.foldCursor + 1,
            prependMidgardCekDataListSummary(child, frame.frame.sequence),
          ),
        );
        control = {
          ...control,
          frameRoot: hashMidgardCekDataScanFrame(frame.frame),
        };
      }
    }

    const summary = structuredSummary(data, frame.frame.sequence);
    emit({
      kind: "finalizeFrame",
      frame: frame.frame,
      parent: parent?.frame ?? null,
    });
    if (parent === null) {
      if (control.offset !== raw.length) {
        throw new Error("CEK structured Data root has trailing bytes");
      }
      control = {
        ...control,
        frameRoot: Buffer.alloc(0),
        frameClosed: false,
        result: summary,
      };
      continue;
    }
    const nextParent = appendChild(parent, summary);
    replaceFrame(parent, nextParent);
    control = {
      ...control,
      frameRoot: hashMidgardCekDataScanFrame(parent.frame),
      frameClosed:
        parent.frame.kind === 3 &&
        parent.frame.childCount === parent.frame.expectedChildren,
    };
  }
  if (control.result === null) {
    throw new Error("CEK Data scanner did not produce a terminal summary");
  }
  return Object.freeze({
    initial,
    steps: Object.freeze(steps),
    terminal: control,
  });
};
