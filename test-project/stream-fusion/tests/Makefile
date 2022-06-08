
GHC_FLAGS=-cpp -DEXTERNAL_PACKAGE -i.. -O2 -package QuickCheck-1.2.0.1 -fsimpl-tick-factor=100000
#GHC_FLAGS="-hide-package arrows-0.2"
GHC=ghc

# run everything!
all :: internal list stream strictness list-compiled stream-compiled internal-compiled strictness-compiled

# ---------------------------------------------------------------
# testing

list ::
	echo main | ghci  $(GHC_FLAGS) Properties/ListVsSpec.hs
	echo main | ghci  $(GHC_FLAGS) Properties/ListVsBase.hs
stream ::
	echo main | ghci  $(GHC_FLAGS) Properties/StreamListVsSpec.hs
	echo main | ghci  $(GHC_FLAGS) Properties/StreamListVsBase.hs
	echo main | ghci  $(GHC_FLAGS) Properties/StreamVsSpecStream.hs
internal ::
	echo main | ghci  $(GHC_FLAGS) Properties/Internal.hs
strictness ::
	echo main | ghci  $(GHC_FLAGS) Strictness/ListVsSpec.hs
	echo main | ghci  $(GHC_FLAGS) Strictness/ListVsBase.hs
	echo main | ghci  $(GHC_FLAGS) Strictness/BaseVsSpec.hs
	echo main | ghci  $(GHC_FLAGS) Strictness/StreamListVsList.hs

list-compiled ::
	ghc  $(GHC_FLAGS) --make -fno-enable-rewrite-rules -o lp Properties/ListVsSpec.hs
	ghc  $(GHC_FLAGS) --make -fno-enable-rewrite-rules -o lp2 Properties/ListVsBase.hs
	./lp
	./lp2
stream-compiled ::
	ghc  $(GHC_FLAGS) --make -fno-enable-rewrite-rules -o sp Properties/StreamListVsSpec.hs
	ghc  $(GHC_FLAGS) --make -fno-enable-rewrite-rules -o sp2 Properties/StreamListVsBase.hs
	ghc  $(GHC_FLAGS) --make -fno-enable-rewrite-rules -o sp3 Properties/StreamVsSpecStream.hs
	./sp
	./sp2
	./sp3
internal-compiled ::
	ghc  $(GHC_FLAGS) --make -fno-enable-rewrite-rules -o ip Properties/Internal.hs
	./ip
strictness-compiled ::
	ghc  $(GHC_FLAGS) --make -fno-enable-rewrite-rules -o strp Strictness/ListVsSpec.hs
	ghc  $(GHC_FLAGS) --make -fno-enable-rewrite-rules -o strp2 Strictness/ListVsBase.hs
	ghc  $(GHC_FLAGS) --make -fno-enable-rewrite-rules -o strp3 Strictness/BaseVsSpec.hs
	ghc  $(GHC_FLAGS) --make -fno-enable-rewrite-rules -o strp4 Strictness/StreamListVsList.hs
	./strp
	./strp2
	./strp3
	./strp4

# ---------------------------------------------------------------
# benchmarking

bench ::
	$(GHC) $(GHC_FLAGS) --make Bench/ListVsBase.hs -o bench
	$(GHC) $(GHC_FLAGS) --make Bench/StreamVsList.hs -o bench2
	./bench
	./bench2

fusetest :: compiled
	$(GHC) $(GHC_FLAGS) -c -no-recomp -ddump-simpl-stats \
		 FuseTest.hs

compiled :: ../Data/Stream.hs ../Data/List/Stream.hs \
	Bench/Utils.hs Properties/Utils.hs Strictness/Utils.hs \
	Spec/List.hs Spec/ListExts.hs Spec/PreludeList.hs \
	Properties/Monomorphic/Base.hs \
	Properties/Monomorphic/List.hs \
	Properties/Monomorphic/Spec.hs \
	Properties/Monomorphic/SpecStream.hs \
	Properties/Monomorphic/Stream.hs \
	Properties/Monomorphic/StreamList.hs \
	Strictness/Monomorphic/Base.hs \
	Strictness/Monomorphic/List.hs \
	Strictness/Monomorphic/Spec.hs \
	Strictness/Monomorphic/StreamList.hs
	$(GHC) $(GHC_FLAGS) --make $^

Bench/bigdata :
	cat Bench/data Bench/data Bench/data Bench/data Bench/data > Bench/bigdata

# ---------------------------------------------------------------
# cleaning

clean :
	rm -f *.hi *.o ../Data/*.o ../Data/*.hi ../Data/List/*.o ../Data/List/*.hi
	rm -f Spec/*.o Spec/*.hi Bench/*.o Bench/*.hi
	rm -f Properties/*.o Properties/*.hi Strictness/*.o Strictness/*.hi
	rm -f bench bench2 lp lp2 sp sp2 sp3 ip strp strp2 strp3
