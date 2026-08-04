import defaultMdxComponents from 'fumadocs-ui/mdx';
import { Card, Cards } from 'fumadocs-ui/components/card';
import { Tab, Tabs } from 'fumadocs-ui/components/tabs';
import { Step, Steps } from 'fumadocs-ui/components/steps';
import type { MDXComponents } from 'mdx/types';
import { Popup, PopupContent, PopupTrigger } from 'fumadocs-twoslash/ui';
import Mermaid from './Mermaid';
import {
  RiskCallout,
  InvariantBox,
  StatePill,
  ActorCard,
  EndpointTable,
} from './protocol';

export function getMDXComponents(components?: MDXComponents): MDXComponents {
  return {
    ...defaultMdxComponents,
    // Hover popups for `ts twoslash` code blocks.
    Popup,
    PopupContent,
    PopupTrigger,
    Card,
    Cards,
    Tab,
    Tabs,
    Step,
    Steps,
    Mermaid,
    RiskCallout,
    InvariantBox,
    StatePill,
    ActorCard,
    EndpointTable,
    ...components,
  };
}

export const useMDXComponents = getMDXComponents;

declare global {
  type MDXProvidedComponents = ReturnType<typeof getMDXComponents>;
}
