import type { MidgardValidationDispute } from "@al-ft/midgard-core/validation-dispute";

import { ValidationDispute } from "./validation-dispute.validation-machine-phase-schema.js";
import {
  bytesHex,
  safeNumber,
  validationTraceDescriptorCoreFromData,
  validationTraceDescriptorDataFromCore,
} from "./validation-dispute.validation-machine-state-data-from-core.js";

export const validationDisputeDataFromCore = (
  dispute: MidgardValidationDispute,
): ValidationDispute => ({
  version: BigInt(dispute.version),
  operator_descriptor: validationTraceDescriptorDataFromCore(
    dispute.operatorDescriptor,
  ),
  challenger_descriptor: validationTraceDescriptorDataFromCore(
    dispute.challengerDescriptor,
  ),
  low_index: BigInt(dispute.lowIndex),
  high_index: BigInt(dispute.highIndex),
  agreed_low_hash: bytesHex(dispute.agreedLowHash),
  operator_high_hash: bytesHex(dispute.operatorHighHash),
  challenger_high_hash: bytesHex(dispute.challengerHighHash),
  round: BigInt(dispute.round),
  response_deadline: BigInt(dispute.responseDeadline),
  turn:
    dispute.turn.type === "awaitingOperator"
      ? {
          AwaitingOperator: { midpoint: BigInt(dispute.turn.midpoint) },
        }
      : dispute.turn.type === "awaitingChallenger"
        ? {
            AwaitingChallenger: {
              midpoint: BigInt(dispute.turn.midpoint),
              operator_midpoint_hash: bytesHex(
                dispute.turn.operatorMidpointHash,
              ),
            },
          }
        : "ReadyForOneStep",
});

export const validationDisputeCoreFromData = (
  dispute: ValidationDispute,
): MidgardValidationDispute => ({
  version: safeNumber(
    dispute.version,
    "dispute.version",
  ) as MidgardValidationDispute["version"],
  operatorDescriptor: validationTraceDescriptorCoreFromData(
    dispute.operator_descriptor,
  ),
  challengerDescriptor: validationTraceDescriptorCoreFromData(
    dispute.challenger_descriptor,
  ),
  lowIndex: safeNumber(dispute.low_index, "dispute.low_index"),
  highIndex: safeNumber(dispute.high_index, "dispute.high_index"),
  agreedLowHash: Buffer.from(dispute.agreed_low_hash, "hex"),
  operatorHighHash: Buffer.from(dispute.operator_high_hash, "hex"),
  challengerHighHash: Buffer.from(dispute.challenger_high_hash, "hex"),
  round: safeNumber(dispute.round, "dispute.round"),
  responseDeadline: safeNumber(
    dispute.response_deadline,
    "dispute.response_deadline",
  ),
  turn:
    dispute.turn === "ReadyForOneStep"
      ? { type: "readyForOneStep" }
      : "AwaitingOperator" in dispute.turn
        ? {
            type: "awaitingOperator",
            midpoint: safeNumber(
              dispute.turn.AwaitingOperator.midpoint,
              "dispute.turn.midpoint",
            ),
          }
        : {
            type: "awaitingChallenger",
            midpoint: safeNumber(
              dispute.turn.AwaitingChallenger.midpoint,
              "dispute.turn.midpoint",
            ),
            operatorMidpointHash: Buffer.from(
              dispute.turn.AwaitingChallenger.operator_midpoint_hash,
              "hex",
            ),
          },
});
