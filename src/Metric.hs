module Metric
  ( -- re-exports
    bitsMetric,
    qubitsMetric,
    gateCountMetric,
    tCountMetric,
    depthMetric,
    widthMetric,
    tDepthMetric,
    -- qasm
    qasmWidthMetric,
    qasmGateCountMetric,
    qasmDepthMetric,
    GlobalMetricModule
      ( desugarIdentity,
        desugarWire,
        desugarSequence,
        desugarParallel,
        desugarBoundedSequence,
        desugarBoundedParallel,
        desugarOperation
      ),
    LocalMetricModule (desugarOutput),
  )
where

import Metric.Global
  ( GlobalMetricModule
      ( desugarBoundedParallel,
        desugarBoundedSequence,
        desugarIdentity,
        desugarOperation,
        desugarParallel,
        desugarSequence,
        desugarWire
      ),
  )
import Metric.Global.Bits (bitsMetric)
import Metric.Global.GateCount (gateCountMetric)
import Metric.Global.Qubits (qubitsMetric)
import Metric.Global.TCount (tCountMetric)
import Metric.Global.Width (widthMetric)
import Metric.Local (LocalMetricModule (desugarOutput))
import Metric.Local.Depth (depthMetric)
import Metric.Local.TDepth (tDepthMetric)
--qasm
import Metric.Global.QasmGateCount (qasmGateCountMetric)
import Metric.Global.QasmWidth (qasmWidthMetric)
import Metric.Local.QasmDepth (qasmDepthMetric)