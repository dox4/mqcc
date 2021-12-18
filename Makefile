SRC=$(wildcard *.cc)
CXXFLAGS=-std=c++2a -g -fPIC -Wall -DDEBUG
# BUILD_DIR=build
# OBJS=$(addprefix $(BUILD_DIR)/, $(SRC:%.cc=%.o))
OBJS=$(SRC:%.cc=%.o)
CXX=g++

# $(BUILD_DIR)/%.o : %.cc %.h $(BUILD_DIR)
# 	$(CXX) $(CXXFLAGS) -c $< -o $@
%.o : %.cc %.h
	$(CXX) $(CXXFLAGS) -c $< -o $@

mqcc: $(OBJS)
	$(CXX) $(CXXFLAGS) $^ -o $@

.PHONY: clean test

test:
	@bash test/simple-test.sh

# clean:
# 	@rm $(BUILD_DIR)/*.o mqcc
clean:
	@rm *.o mqcc
