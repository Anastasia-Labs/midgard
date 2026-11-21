import {
  deterministicFixtureBytes,
  deterministicFixtureOutputReferenceId,
  deterministicFixtureTxHash,
} from "../utils.js";

export const databaseOutputReferenceId = (
  label: string,
  outputIndex: number | bigint = 0n,
): Buffer =>
  deterministicFixtureOutputReferenceId(`database:${label}`, outputIndex);

export const databaseFixtureBytes = (label: string, length: number): Buffer =>
  deterministicFixtureBytes(`database:${label}`, length);

export const databaseTxHash = (label: string): Buffer =>
  deterministicFixtureTxHash(`database:${label}`);
