module Metric 
( --re-exports
  bitsMetric,
  qubitsMetric,
  gateCountMetric,
  tCountMetric,
  depthMetric,
  widthMetric,
  tDepthMetric,
  GlobalMetricModule( desugarIdentity,
    desugarWire,
    desugarSequence,
    desugarParallel,
    desugarBoundedSequence,
    desugarBoundedParallel,
    desugarOperation),
  LocalMetricModule(desugarOutput),
  
) where

import Metric.Global
import Metric.Local
import Metric.Global.Bits
import Metric.Global.Qubits
import Metric.Global.GateCount
import Metric.Global.TCount
import Metric.Global.Width
import Metric.Local.Depth
import Metric.Local.TDepth
