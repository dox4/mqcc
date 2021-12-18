#ifndef _MQCC_OBJ_POOL_H__
#define _MQCC_OBJ_POOL_H__

#include <cstddef>
#include <cstdint>
template <typename T> class ObjPool {
  public:
    explicit ObjPool<T>() {}
    explicit ObjPool<T>(ObjPool<T> &&) = delete;
    ObjPool<T> &operator=(ObjPool<T> &&) = delete;

  private:
    static constexpr auto _CHUNK_SIZE = sizeof(T);
    static constexpr auto _CHUNK_COUNT = (4 * 1024) / _CHUNK_SIZE;
    union Chunk {
        Chunk *next;
        char _mem[_CHUNK_SIZE];
    };
    Chunk *_free_list;
};

#endif
