if $(ormolu --mode check $(find . -name '*.hs')); then
    echo "Gram is formatted correctly"
else
    echo "Please run ormolu"
fi
