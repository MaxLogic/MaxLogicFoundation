unit FixedCriticalSection;

interface
uses
        windows, syncObjs;


          { TFixedCriticalSection (along with TMonitor*) suffers from a severe design flaw in which entering/leaving different TFixedCriticalSection instances can end up serializing your threads, and the whole can even end up performing worse than if your threads had been serialized.
    This is because it’s a small, dynamically allocated object, so several TFixedCriticalSection instances can end up in the same CPU cache line, and when that happens, you’ll have cache conflicts aplenty between the cores running the threads.
    How severe can that be? Well, it depends on how many cores you have, but the more cores you have, the more severe it can get. On a quad core, a bad case of contention can easily result in a 200% slowdown on top of the serialization. And it won’t always be reproducible, since it’s related to dynamic memory allocation.
    There is thankfully a simple fix for that, use TFixedCriticalSection: }

Type
  TFixedCriticalSection = class(TCriticalSection)
  private
    FDummy: array [0 .. 95] of Byte;
  end;


implementation

end.
