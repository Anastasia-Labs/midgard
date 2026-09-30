import {
  type OpenLoopCorpusShape,
  type OpenLoopWorkloadProfile,
} from "../stress-open-loop.js";
import { type E2EL2StressLoadModel, type E2EL2StressMode } from "./types.js";

export const parsePositiveInteger = (
  value: string | undefined,
  label: string,
  defaultValue: number,
): number => {
  const raw = value?.trim();
  if (raw === undefined || raw.length === 0) {
    return defaultValue;
  }
  if (!/^\d+$/.test(raw)) {
    throw new Error(`${label} must be a positive integer.`);
  }
  const parsed = Number(raw);
  if (!Number.isSafeInteger(parsed) || parsed <= 0) {
    throw new Error(`${label} must be a safe positive integer.`);
  }
  return parsed;
};

export const parseNonNegativeInteger = (
  value: string | undefined,
  label: string,
  defaultValue: number,
): number => {
  const raw = value?.trim();
  if (raw === undefined || raw.length === 0) {
    return defaultValue;
  }
  if (!/^\d+$/.test(raw)) {
    throw new Error(`${label} must be a non-negative integer.`);
  }
  const parsed = Number(raw);
  if (!Number.isSafeInteger(parsed) || parsed < 0) {
    throw new Error(`${label} must be a safe non-negative integer.`);
  }
  return parsed;
};

export const parsePositiveNumber = (
  value: string | undefined,
  label: string,
  defaultValue: number,
): number => {
  const raw = value?.trim();
  if (raw === undefined || raw.length === 0) {
    return defaultValue;
  }
  const parsed = Number(raw);
  if (!Number.isFinite(parsed) || parsed <= 0) {
    throw new Error(`${label} must be a positive number.`);
  }
  return parsed;
};

export const parsePositiveBigInt = (
  value: string | undefined,
  label: string,
  defaultValue: bigint,
): bigint => {
  const raw = value?.trim();
  if (raw === undefined || raw.length === 0) {
    return defaultValue;
  }
  if (!/^\d+$/.test(raw)) {
    throw new Error(`${label} must be a positive integer.`);
  }
  const parsed = BigInt(raw);
  if (parsed <= 0n) {
    throw new Error(`${label} must be greater than zero.`);
  }
  return parsed;
};

export const parseNonNegativeBigInt = (
  value: string | undefined,
  label: string,
  defaultValue: bigint,
): bigint => {
  const raw = value?.trim();
  if (raw === undefined || raw.length === 0) {
    return defaultValue;
  }
  if (!/^\d+$/.test(raw)) {
    throw new Error(`${label} must be a non-negative integer.`);
  }
  return BigInt(raw);
};

export const parseMode = (value: string | undefined): E2EL2StressMode => {
  const normalized = value?.trim() || "serial-chain";
  if (normalized === "serial-chain" || normalized === "parallel-fanout") {
    return normalized;
  }
  throw new Error(
    `--mode must be "serial-chain" or "parallel-fanout", got "${value}".`,
  );
};

export const parseLoadModel = (
  value: string | undefined,
): E2EL2StressLoadModel => {
  const normalized = value?.trim() || "closed-loop-smoke";
  if (
    normalized === "closed-loop-smoke" ||
    normalized === "open-loop-upper-bound"
  ) {
    return normalized;
  }
  throw new Error(
    `--load-model must be "closed-loop-smoke" or "open-loop-upper-bound", got "${value}".`,
  );
};

export const parseWorkloadProfile = ({
  value,
  loadModel,
}: {
  readonly value: string | undefined;
  readonly loadModel: E2EL2StressLoadModel;
}): OpenLoopWorkloadProfile => {
  const normalized =
    value?.trim() ||
    (loadModel === "open-loop-upper-bound"
      ? "synthetic-admission"
      : "production-end-user");
  if (
    normalized === "synthetic-admission" ||
    normalized === "production-end-user"
  ) {
    return normalized;
  }
  throw new Error(
    `--workload-profile must be "synthetic-admission" or "production-end-user", got "${value}".`,
  );
};

export const parseCorpusShape = (
  value: string | undefined,
): OpenLoopCorpusShape => {
  const normalized = value?.trim() || "fanout";
  if (
    normalized === "fanout" ||
    normalized === "chain" ||
    normalized === "mixed"
  ) {
    return normalized;
  }
  throw new Error(
    `--corpus-shape must be "fanout", "chain", or "mixed", got "${value}".`,
  );
};
