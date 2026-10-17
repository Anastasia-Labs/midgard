/** Exact twin of one counted direct/published opening. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";
import { H32Schema } from "../common.js";
import {
  EventKeySchema,
  EventToStepValueSchema,
  TransitionStepSchema,
} from "../ledger-state.js";
import { RootDomainSchema } from "../transition-trace.js";
import { MembershipCarriageSchema } from "./native.js";

type DataSchema = Parameters<typeof Data.Nullable>[0];

export const carriedCountedMembershipSchema = <
  K extends DataSchema,
  V extends DataSchema,
>(
  key: K,
  value: V,
) =>
  Data.Object({
    domain: RootDomainSchema,
    root: H32Schema,
    phas_root: H32Schema,
    count: Data.Integer(),
    key,
    value,
    carriage: MembershipCarriageSchema,
  });

export const EventToStepCarriedMembershipSchema =
  carriedCountedMembershipSchema(EventKeySchema, EventToStepValueSchema);
export type EventToStepCarriedMembership = Data.Static<
  typeof EventToStepCarriedMembershipSchema
>;
export const EventToStepCarriedMembership =
  asDataType<EventToStepCarriedMembership>(EventToStepCarriedMembershipSchema);

export const TransitionCarriedMembershipSchema = carriedCountedMembershipSchema(
  Data.Integer(),
  TransitionStepSchema,
);
export type TransitionCarriedMembership = Data.Static<
  typeof TransitionCarriedMembershipSchema
>;
export const TransitionCarriedMembership =
  asDataType<TransitionCarriedMembership>(TransitionCarriedMembershipSchema);
