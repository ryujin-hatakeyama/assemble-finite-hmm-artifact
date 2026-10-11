# Finite-HMM artifact checks.

OCAMLOPT ?= ocamlfind ocamlopt
FLAGS    ?= -O3
BUILD := _build

COMMON := common/finite_domain.mli common/finite_domain.ml common/parameter.ml common/polynomial.ml common/weight.ml \
          common/model_language.ml common/grid_sensor.ml
INTERPRETERS := interpreters/direct_evaluator.ml interpreters/ordered_dist.ml \
                interpreters/indexed_dist.ml interpreters/support_dist.ml interpreters/direct_forward.ml
MODEL_KERNEL := model_kernel/model_kernel.mli model_kernel/model_kernel.ml model_kernel/kernel_interpreter.ml \
                model_kernel/kernel_inference_prob.ml model_kernel/kernel_inference_log.ml
STAGED_MODEL_KERNEL := model_kernel/kernel_staged.ml
LIB := $(COMMON) $(INTERPRETERS) $(MODEL_KERNEL) tests/check.ml

.PHONY: check test staged-check aggregation-scope-check \
	description-invariance-check model-generated-aggregation-scope \
	verify-model-generated-aggregation-scope verify-archived-performance \
	bench-kernel-evaluation bench-kernel-evaluation-native clean

check: test

$(BUILD)/stamp: $(LIB) tests/test_finite_hmm.ml
	mkdir -p $(BUILD)
	cp $^ $(BUILD)/
	touch $@

define link
	cd $(BUILD) && $(OCAMLOPT) $(FLAGS) -w +a-4-9-40-41-42-44-45-70 \
	  $(notdir $(1)) -o $(2)
endef

$(BUILD)/test_finite_hmm.exe: $(BUILD)/stamp
	$(call link,$(LIB) tests/test_finite_hmm.ml,test_finite_hmm.exe)

test: $(BUILD)/test_finite_hmm.exe
	$(BUILD)/test_finite_hmm.exe
	sh tests/check_finite_domain_abstraction.sh $(BUILD)


AGGREGATION_BUILD := _build_aggregation_scope
AGGREGATION_LIB := $(COMMON) model_kernel/model_kernel.ml \
                   model_kernel/kernel_interpreter.ml \
                   model_kernel/kernel_inference_prob.ml tests/check.ml

$(AGGREGATION_BUILD)/test_aggregation_scope.exe: $(AGGREGATION_LIB) tests/test_aggregation_scope.ml
	mkdir -p $(AGGREGATION_BUILD)
	cp $^ $(AGGREGATION_BUILD)/
	cd $(AGGREGATION_BUILD) && $(OCAMLOPT) $(FLAGS) \
	  -w +a-4-9-40-41-42-44-45-70 $(notdir $(AGGREGATION_LIB)) \
	  test_aggregation_scope.ml -o test_aggregation_scope.exe

aggregation-scope-check: $(AGGREGATION_BUILD)/test_aggregation_scope.exe
	$(AGGREGATION_BUILD)/test_aggregation_scope.exe


DESCRIPTION_INVARIANCE_BUILD := _build_description_invariance
DESCRIPTION_INVARIANCE_LIB := $(COMMON) interpreters/direct_evaluator.ml \
				      interpreters/indexed_dist.ml \
			      model_kernel/model_kernel.ml \
			      model_kernel/kernel_interpreter.ml \
			      model_kernel/kernel_inference_prob.ml \
			      tests/trace_max_dist.ml tests/check.ml

$(DESCRIPTION_INVARIANCE_BUILD)/test_description_invariance.exe: \
    $(DESCRIPTION_INVARIANCE_LIB) tests/test_description_invariance.ml
	mkdir -p $(DESCRIPTION_INVARIANCE_BUILD)
	cp $^ $(DESCRIPTION_INVARIANCE_BUILD)/
	cd $(DESCRIPTION_INVARIANCE_BUILD) && $(OCAMLOPT) $(FLAGS) \
	  -w +a-4-9-40-41-42-44-45-70 $(notdir $(DESCRIPTION_INVARIANCE_LIB)) \
	  test_description_invariance.ml -o test_description_invariance.exe

description-invariance-check: \
    $(DESCRIPTION_INVARIANCE_BUILD)/test_description_invariance.exe
	$(DESCRIPTION_INVARIANCE_BUILD)/test_description_invariance.exe


MODEL_GENERATED_BUILD := _build_model_generated_aggregation
MODEL_GENERATED_LIB := $(COMMON) interpreters/direct_evaluator.ml \
                       interpreters/indexed_dist.ml \
                       model_kernel/model_kernel.ml \
                       model_kernel/kernel_interpreter.ml \
                       model_kernel/kernel_inference_prob.ml \
                       tests/trace_max_dist.ml

$(MODEL_GENERATED_BUILD)/experiment_model_generated_aggregation.exe: \
    $(MODEL_GENERATED_LIB) tests/experiment_model_generated_aggregation.ml
	mkdir -p $(MODEL_GENERATED_BUILD)
	cp $^ $(MODEL_GENERATED_BUILD)/
	cd $(MODEL_GENERATED_BUILD) && $(OCAMLOPT) $(FLAGS) -package unix -linkpkg \
	  -w +a-4-9-40-41-42-44-45-70 $(notdir $(MODEL_GENERATED_LIB)) \
	  experiment_model_generated_aggregation.ml -o experiment_model_generated_aggregation.exe

model-generated-aggregation-scope: \
    $(MODEL_GENERATED_BUILD)/experiment_model_generated_aggregation.exe
	$(MODEL_GENERATED_BUILD)/experiment_model_generated_aggregation.exe

verify-model-generated-aggregation-scope: \
    $(MODEL_GENERATED_BUILD)/experiment_model_generated_aggregation.exe \
    tests/verify_model_generated_aggregation.sh
	tests/verify_model_generated_aggregation.sh

verify-archived-performance: bench/verify_archived_performance.sh
	bench/verify_archived_performance.sh


STAGED_BUILD := _build_staged
NATIVE_STAGED_BUILD := _build_staged_native
METAOCAMLC ?= ocamlfind ocamlc
METAOCAMLOPT ?= ocamlfind ocamlopt
METAOCAML_PKGS ?= -package metaocaml
METAOCAML_FLAGS ?= -g -linkpkg
METAOCAMLOPT_FLAGS ?= -O3 -linkpkg
NATIVE_RUNCODE_SHIM := bench/native_runcode_shim.ml
STAGED_LIB := $(COMMON) interpreters/direct_evaluator.ml interpreters/indexed_dist.ml \
              model_kernel/model_kernel.mli model_kernel/model_kernel.ml model_kernel/kernel_interpreter.ml \
              model_kernel/kernel_inference_prob.ml $(STAGED_MODEL_KERNEL) tests/check.ml
STAGED_TEST_SUPPORT := bench/handwritten_packed_inference.ml

$(STAGED_BUILD)/stamp: $(STAGED_LIB) $(STAGED_TEST_SUPPORT) tests/test_kernel_staged.ml
	mkdir -p $(STAGED_BUILD)
	cp $^ $(STAGED_BUILD)/
	touch $@

$(STAGED_BUILD)/test_kernel_staged.byte: $(STAGED_BUILD)/stamp
	cd $(STAGED_BUILD) && $(METAOCAMLC) $(METAOCAML_PKGS) $(METAOCAML_FLAGS) \
	  -w +a-4-9-40-41-42-44-45-70 $(notdir $(STAGED_LIB)) \
	  $(notdir $(STAGED_TEST_SUPPORT)) test_kernel_staged.ml \
	  -o test_kernel_staged.byte

staged-check: $(STAGED_BUILD)/test_kernel_staged.byte
	$(STAGED_BUILD)/test_kernel_staged.byte

$(STAGED_BUILD)/bench_kernel_evaluation.byte: $(STAGED_LIB) bench/handwritten_packed_inference.ml bench/bench_kernel_evaluation.ml
	mkdir -p $(STAGED_BUILD)
	cp $(STAGED_LIB) bench/handwritten_packed_inference.ml bench/bench_kernel_evaluation.ml $(STAGED_BUILD)/
	cd $(STAGED_BUILD) && $(METAOCAMLC) -package metaocaml,unix $(METAOCAML_FLAGS) \
	  -w +a-4-9-40-41-42-44-45-70 $(notdir $(STAGED_LIB)) handwritten_packed_inference.ml bench_kernel_evaluation.ml \
	  -o bench_kernel_evaluation.byte

bench-kernel-evaluation: $(STAGED_BUILD)/bench_kernel_evaluation.byte

$(NATIVE_STAGED_BUILD)/bench_kernel_evaluation.native: $(STAGED_LIB) bench/handwritten_packed_inference.ml bench/bench_kernel_evaluation.ml $(NATIVE_RUNCODE_SHIM)
	mkdir -p $(NATIVE_STAGED_BUILD)
	cp $(STAGED_LIB) bench/handwritten_packed_inference.ml bench/bench_kernel_evaluation.ml $(NATIVE_RUNCODE_SHIM) $(NATIVE_STAGED_BUILD)/
	mv $(NATIVE_STAGED_BUILD)/native_runcode_shim.ml $(NATIVE_STAGED_BUILD)/runcode.ml
	cd $(NATIVE_STAGED_BUILD) && $(METAOCAMLOPT) -package metaocaml,dynlink,unix $(METAOCAMLOPT_FLAGS) \
	  -w +a-4-9-40-41-42-44-45-70 runcode.ml $(notdir $(STAGED_LIB)) handwritten_packed_inference.ml bench_kernel_evaluation.ml \
	  -o bench_kernel_evaluation.native

bench-kernel-evaluation-native: $(NATIVE_STAGED_BUILD)/bench_kernel_evaluation.native

clean:
	rm -rf $(BUILD) $(STAGED_BUILD) $(NATIVE_STAGED_BUILD) $(AGGREGATION_BUILD) \
	  $(DESCRIPTION_INVARIANCE_BUILD) $(MODEL_GENERATED_BUILD) bench/__pycache__
